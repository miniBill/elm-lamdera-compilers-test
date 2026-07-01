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
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script as Script exposing (Script, makeDirectory)
import Path exposing (Path)
import Time
import Tui
import Tui.Effect as Effect exposing (Effect)
import Tui.Screen as Screen exposing (Screen)
import Tui.Sub
import Url.Builder


type DownloadError
    = FailedToRemoveDirectory String
    | FailedToRemoveFile String
    | FailedToMakeDirectory String
    | FailedToExecute String
    | FailedToStatFile String
    | DownloadedEmptyFile String String
    | FailedToExtractRootFolder String


type alias Package =
    { author : String
    , name : String
    , version : String
    }


run : Script
run =
    Script.withCliOptions programConfig toTask


programConfig : Program.Config (Config (List Package))
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build
                (Config
                    (BackendTask.allowFatal getPackageList)
                    buildAction
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
        Do.do (BuildTask.run { jobs = config.jobs, debug = config.debug, hashKind = config.hashKind } config.buildDirectory (config.buildAction inputs)) <| \combined ->
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


buildAction : List Package -> BuildTask FileOrDirectory
buildAction packages =
    let
        packagesCount : Int
        packagesCount =
            List.length packages
    in
    packages
        |> List.indexedMap
            (\index package ->
                let
                    prefix : String
                    prefix =
                        "["
                            ++ String.padLeft (String.length (String.fromInt packagesCount)) '0' (String.fromInt index)
                            ++ "/"
                            ++ String.fromInt packagesCount
                            ++ "]"
                in
                downloadPackage package
                    |> BuildTask.withPrefix prefix
                    |> BuildTask.map
                        (\( downloaded, hasTests ) ->
                            { filename = Path.path (String.join "/" [ package.author, package.name, package.version ])
                            , hash = downloaded
                            }
                        )
            )
        |> BuildTask.combine
        |> BuildTask.andThen BuildTask.combineInto


getPackageList : BackendTask { fatal : FatalError, recoverable : Http.Error } (List Package)
getPackageList =
    -- 0.19 cutoff
    Http.get "https://package.elm-lang.org/all-packages/since/6557"
        (Http.expectJson packageListDecoder)


packageListDecoder : Json.Decode.Decoder (List Package)
packageListDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                case String.split "@" raw of
                    [ before, version ] ->
                        case String.split "/" before of
                            [ author, name ] ->
                                Json.Decode.succeed
                                    { author = author
                                    , name = name
                                    , version = version
                                    }

                            _ ->
                                let
                                    msg : String
                                    msg =
                                        "Could not split package author and name in " ++ escape raw
                                in
                                Json.Decode.fail msg

                    _ ->
                        let
                            msg : String
                            msg =
                                "Could not split package author/name and version in " ++ escape raw
                        in
                        Json.Decode.fail msg
            )
        |> Json.Decode.list


downloadPackage : Package -> BuildTask ( FileOrDirectory, Bool )
downloadPackage package =
    let
        targetFolder : String
        targetFolder =
            String.join "/"
                [ "repos"
                , package.author
                , package.name
                , package.version
                ]

        url : String
        url =
            Url.Builder.crossOrigin "https://github.com"
                [ package.author
                , package.name
                , "archive"
                , "refs"
                , "tags"
                , package.version ++ ".tar.gz"
                ]
                []

        getRoot : List String -> BuildTask String
        getRoot contents =
            case contents of
                firstLine :: _ ->
                    case String.split "/" firstLine of
                        [ root, "" ] ->
                            BuildTask.succeed root

                        _ ->
                            BuildTask.fail ("Unexpected first content line: " ++ escape firstLine)

                _ ->
                    -- Empty file, root is irrelevant
                    BuildTask.succeed ""
    in
    BuildTask.Unsafe.Do.downloadImmutable url <| \tarGz ->
    BuildTask.Unsafe.Do.pipeThrough "gunzip" [] tarGz <| \tar ->
    BuildTask.do (BuildTask.Tar.listContents tar) <| \contents ->
    BuildTask.do (getRoot contents) <| \root ->
    let
        hasTests : Bool
        hasTests =
            List.member (root ++ "/tests/") contents
    in
    BuildTask.do
        (if hasTests then
            BuildTask.Tar.extract { stripPrefix = Just root } tar [ "elm.json", "src", "tests" ]

         else
            BuildTask.Tar.extract { stripPrefix = Just root } tar [ "elm.json", "src" ]
        )
    <| \result ->
    BuildTask.succeed ( result, hasTests )


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)



-- BuildTask.Tar.extractFiles tarGz [ "elm.json", "src" ]
-- BuildTask.do BuildTask.Unsafe.commandInWritableDirectory <| \tar ->
-- Do.do (doDownloadPackage package) <| \_ ->
-- File.exists (targetFolder ++ "/test")


doDownloadPackage : Package -> BackendTask DownloadError ()
doDownloadPackage package =
    let
        url : String
        url =
            Url.Builder.crossOrigin "https://github.com"
                [ package.author
                , package.name
                , "archive"
                , "refs"
                , "tags"
                , package.version ++ ".tar.gz"
                ]
                []

        gzFilename : String
        gzFilename =
            String.join "/"
                [ "tmp"
                , package.author
                , package.name
                , package.version ++ ".tar.gz"
                ]

        tmpFolder : String
        tmpFolder =
            String.join "/"
                [ "tmp"
                , package.author
                , package.name
                , package.version
                ]

        targetFolderContainer : String
        targetFolderContainer =
            String.join "/"
                [ "repos"
                , package.author
                , package.name
                ]

        targetFolder : String
        targetFolder =
            String.join "/"
                [ "repos"
                , package.author
                , package.name
                , package.version
                ]

        tarArgs : String -> List String -> List String
        tarArgs tarRoot files =
            [ "xzf"
            , gzFilename
            , "--strip-components"
            , "1"
            , "-C"
            , tmpFolder
            ]
                ++ List.map
                    (\file -> tarRoot ++ "/" ++ file)
                    files

        removeDirectoryRecursive : String -> BackendTask DownloadError ()
        removeDirectoryRecursive dir =
            Script.removeDirectory { recursive = True } dir
                |> BackendTask.mapError (\_ -> FailedToRemoveDirectory dir)

        removeFile : String -> BackendTask DownloadError ()
        removeFile file =
            Script.removeFile file
                |> BackendTask.mapError (\_ -> FailedToRemoveFile file)

        makeDirectoryRecursive : String -> BackendTask DownloadError ()
        makeDirectoryRecursive dir =
            Script.makeDirectory { recursive = True } dir
                |> BackendTask.mapError (\_ -> FailedToMakeDirectory dir)

        exec : String -> List String -> BackendTask DownloadError ()
        exec cmd args =
            let
                maybeQuote : String -> String
                maybeQuote s =
                    if String.contains " " s then
                        escape s

                    else
                        s
            in
            Script.exec cmd args
                |> BackendTask.mapError (\_ -> FailedToExecute (String.join " " (List.map maybeQuote (cmd :: args))))

        stat : String -> BackendTask DownloadError Glob.FileStats
        stat file =
            Glob.succeed identity
                |> Glob.match (Glob.literal file)
                |> Glob.captureStats
                |> Glob.expectUniqueMatch
                |> BackendTask.mapError (\_ -> FailedToStatFile file)

        getTarRoot : BackendTask DownloadError String
        getTarRoot =
            Script.command "tar" [ "tf", gzFilename ]
                |> BackendTask.mapError (\_ -> FailedToExtractRootFolder ("Failed to run tar tf for " ++ gzFilename))
                |> BackendTask.andThen
                    (\raw ->
                        case String.lines raw of
                            head :: _ ->
                                case String.split "/" head of
                                    root :: _ ->
                                        BackendTask.succeed root

                                    [] ->
                                        BackendTask.fail (FailedToExtractRootFolder ("Failed to run get root from the first file for " ++ gzFilename))

                            [] ->
                                -- File is empty anyway, it will fail
                                BackendTask.succeed ""
                    )
    in
    Do.do (removeDirectoryRecursive tmpFolder) <| \_ ->
    Do.do (removeFile gzFilename) <| \_ ->
    Do.do (makeDirectoryRecursive tmpFolder) <| \_ ->
    Do.do (exec "curl" [ "-sSL", url, "--remove-on-error", "-o", gzFilename ]) <| \_ ->
    Do.do (stat gzFilename) <| \filenameStats ->
    if filenameStats.sizeInBytes == 0 then
        BackendTask.fail (DownloadedEmptyFile url gzFilename)

    else
        Do.do getTarRoot <| \tarRoot ->
        Do.do (makeDirectoryRecursive tmpFolder) <| \_ ->
        Do.do (exec "tar" (tarArgs tarRoot [ "elm.json", "src" ])) <| \_ ->
        Do.do
            (exec "tar" (tarArgs tarRoot [ "tests" ])
                -- Ignore errors here
                |> BackendTask.toResult
            )
        <| \_ ->
        Do.do (makeDirectoryRecursive targetFolderContainer) <| \_ ->
        Do.do (exec "mv" [ tmpFolder, targetFolder ]) <| \_ ->
        Do.do (removeFile gzFilename) <| \_ ->
        Do.do (removeDirectoryRecursive tmpFolder) <| \_ ->
        Do.noop


packagesToString : { author : String, name : String, versions : List String } -> String
packagesToString { author, name, versions } =
    author ++ "/" ++ name ++ " " ++ String.join " " versions


mapLonger : (Maybe a -> Maybe b -> p) -> List a -> List b -> List p
mapLonger f l r =
    mapLongerHelp f l r []


mapLongerHelp : (Maybe a -> Maybe b -> c) -> List a -> List b -> List c -> List c
mapLongerHelp f l r acc =
    case l of
        [] ->
            case r of
                [] ->
                    List.reverse acc

                rHead :: rTail ->
                    mapLongerHelp f l rTail (f Nothing (Just rHead) :: acc)

        lHead :: lTail ->
            case r of
                [] ->
                    mapLongerHelp f lTail r (f (Just lHead) Nothing :: acc)

                rHead :: rTail ->
                    mapLongerHelp f lTail rTail (f (Just lHead) (Just rHead) :: acc)


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ ":" ++ package.version
