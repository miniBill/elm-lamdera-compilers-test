module Main exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import BackendTask.Time
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Tar
import BuildTask.Unsafe
import BuildTask.Unsafe.Do
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import CompilerTestBuildfile exposing (Package)
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script as Script exposing (Script, makeDirectory)
import Path exposing (Path)
import Time
import Url.Builder


run : Script
run =
    Script.withCliOptions programConfig toTask


programConfig : Program.Config (Config (List Package))
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build
                (Config
                    CompilerTestBuildfile.getInput
                    CompilerTestBuildfile.buildAction
                )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.withDefault "build"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Build directory - contains the intermediate files"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "output"
                        |> Option.withDefault "out"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Output directory"
                    )
                |> OptionsParser.with
                    (Option.flag "remove-stale"
                        |> Option.withDescription "Remove unused files from the build directory"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "jobs"
                        |> Option.withDescription "Number of parallel jobs to run"
                        |> Option.withDisplayName "n"
                        |> Option.validateMapIfPresent
                            (\j ->
                                case String.toInt j of
                                    Nothing ->
                                        Err ("Invalid number of jobs: " ++ j)

                                    Just i ->
                                        Ok i
                            )
                    )
                |> OptionsParser.with
                    (Option.flag "debug"
                        |> Option.withDescription "Output debug info"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "hash-kind"
                        |> Option.withDescription "Kind of hash to use. Choose fast for FNV1a, secure for sha256."
                        |> Option.withDefault "fast"
                        |> Option.oneOf [ ( "fast", Hash.Fast ), ( "secure", Hash.Secure ) ]
                    )
            )


type alias Config inputs =
    { getInputs : BackendTask FatalError inputs
    , buildAction : inputs -> BuildTask FileOrDirectory
    , buildDirectory : Path
    , outputName : Path
    , removeStale : Bool
    , jobs : Maybe Int
    , debug : Bool
    , hashKind : Hash.Kind
    }


toTask : Config inputs -> BackendTask FatalError ()
toTask config =
    BackendTask.Extra.profiling "main" <|
        Do.do BackendTask.Time.now <| \begin ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Getting inputs") <| \_ ->
        Do.do config.getInputs <| \inputs ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Processing inputs") <| \_ ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        Do.do (BuildTask.run { check = False, jobs = config.jobs, debug = config.debug, hashKind = config.hashKind } config.buildDirectory (config.buildAction inputs)) <| \combined ->
        Do.exec "rm" [ "-f", Path.toString config.outputName ] <| \_ ->
        symlink_
            { source = config.outputName
            , target =
                Path.relativeTo
                    (Path.directory config.outputName)
                    combined.output
            }
        <| \_ ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Output: " ++ Path.toString combined.output) <| \_ ->
        Do.do (BackendTask.Customs.readdir config.buildDirectory) <| \actualList ->
        let
            expected : Set String
            expected =
                combined.intermediate
                    |> List.map Path.toString
                    |> Set.fromList

            actual : Set String
            actual =
                actualList
                    |> List.map (\file -> Path.toString config.buildDirectory ++ "/" ++ file)
                    |> Set.fromList

            unexpected : Set String
            unexpected =
                Set.diff actual expected
        in
        if config.removeStale then
            Do.log ("Removing " ++ String.fromInt (Set.size unexpected) ++ " files from the build directory") <| \_ ->
            Do.do
                (Set.toList unexpected
                    |> List.map
                        (\i ->
                            Do.exec "chmod" [ "-R", "700", i ] <| \_ ->
                            Script.exec "rm" [ "-rf", i ]
                        )
                    |> BackendTask.Extra.sequence_
                )
            <| \_ ->
            Do.do BackendTask.Time.now <| \end ->
            let
                elapsed : Int
                elapsed =
                    Time.posixToMillis end - Time.posixToMillis begin

                msg =
                    "Build done in "
                        ++ timeToString elapsed
                        ++ " with "
                        ++ String.fromInt (Set.size combined.warnings)
                        ++ " "
                        ++ plural (Set.size combined.warnings) "warning" "warnings"
            in
            Script.log msg

        else
            Script.log (String.fromInt (Set.size unexpected) ++ " stale files in the build directory")


plural : Int -> String -> String -> String
plural n singular plural_ =
    if n == 1 then
        singular

    else
        plural_


symlink_ : { source : Path, target : Path } -> (() -> BackendTask FatalError a) -> BackendTask FatalError a
symlink_ config k =
    Do.do (symlink config) k


symlink : { source : Path, target : Path } -> BackendTask FatalError ()
symlink { source, target } =
    Script.exec "ln" [ "-s", Path.toString target, Path.toString source ]


timeToString : Int -> String
timeToString ms =
    let
        s : Int
        s =
            ms // 1000

        m : Int
        m =
            s // 60
    in
    if m > 0 then
        String.fromInt m ++ "m " ++ String.fromInt (modBy 60 s) ++ "s " ++ String.fromInt (modBy 1000 ms) ++ "ms"

    else if s > 0 then
        String.fromFloat (toFloat ms / 1000) ++ "s"

    else
        String.fromInt ms ++ "ms"
