module CompilerTestBuildfile exposing (Inputs, Package, Tools, buildAction, getInput, getTools)

import Ansi.Color
import Ansi.Font
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, Command, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Gzip
import BuildTask.Internal as Internal
import BuildTask.Tar
import BuildTask.Unsafe
import Bytes.Encode
import CommandOptions
import Dict as CoreDict
import Dict.Extra
import Diff
import Diff.ToString
import Duration
import Elm.Constraint as Constraint exposing (Constraint)
import Elm.Package as Package
import Elm.Project as Project
import Elm.Version as Version
import ErrorParser
import FNV1a
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Hash
import Hex
import Hex.Convert
import Json.Decode
import Json.Encode
import Length exposing (Length)
import List.Extra
import Maybe.Extra
import Pages.Script as Script
import Path exposing (Path)
import Regex exposing (Regex)
import Result.Extra
import Set
import String.Extra
import Url.Builder
import Utils
import XBytes
import Xlsx


type alias Package =
    { author : String
    , name : String
    , version : String
    }


type alias Inputs =
    { missing : List Package
    , packages : List Package
    , pwd : String
    }


type alias Tools =
    { elm_test : Command
    , elm_test_rs : Command
    , gzip : Command
    , tar : Command
    , compilers : Dict String Command
    }


getInput : BackendTask FatalError Inputs
getInput =
    BackendTask.map3
        (\missing packages pwd ->
            { missing = missing
            , packages = packages
            , pwd = String.trim pwd
            }
        )
        (File.rawFile "missing"
            |> BackendTask.toResult
            |> BackendTask.andThen
                (\res ->
                    case res of
                        Err e ->
                            case e.recoverable of
                                File.FileDoesntExist ->
                                    BackendTask.succeed []

                                _ ->
                                    BackendTask.fail e.fatal

                        Ok raw ->
                            raw
                                |> String.lines
                                |> Result.Extra.combineMap packageFromString
                                |> Result.mapError FatalError.fromString
                                |> BackendTask.fromResult
                )
        )
        -- 0.19 cutoff
        (Http.getWithOptions
            { url = "https://package.elm-lang.org/all-packages/since/6557"
            , expect = Http.expectJson packageListDecoder
            , cachePath = Just ".elm-pages/http-response-cache"
            , cacheStrategy = Just Http.ForceCache
            , headers = []
            , retries = Nothing
            , timeoutInMs = Just 3000
            }
            |> BackendTask.allowFatal
        )
        (Script.command "pwd" [])


buildAction : Inputs -> BuildTask Tools FatalError FileOrDirectory
buildAction { missing, packages, pwd } =
    let
        packagesCount : Int
        packagesCount =
            List.length packages
    in
    packages
        |> List.sortBy packageToString
        |> List.indexedMap
            (\index package ->
                if List.member package missing then
                    BuildTask.succeed Nothing

                else
                    let
                        prefix : String
                        prefix =
                            "["
                                ++ String.padLeft (String.length (String.fromInt packagesCount)) '0' (String.fromInt index)
                                ++ "/"
                                ++ String.fromInt packagesCount
                                ++ "] "
                                ++ packageToString package
                                ++ " "
                    in
                    handlePackage { pwd = pwd } package
                        |> BuildTask.map (\r -> Just ( package, r ))
                        |> BuildTask.withPrefix prefix
            )
        |> BuildTask.combine
        |> BuildTask.andThen
            (\list ->
                let
                    resultsHeader : List ( String, Maybe Float )
                    resultsHeader =
                        ( "Author", Just 19.24 )
                            :: ( "Package", Just 30 )
                            :: ( "Version", Just 10.48 )
                            :: ( "Result", Just 27.59 )
                            :: List.map (\compiler -> ( compiler, Just 70 )) allCompilers

                    resultLines : List (List String)
                    resultLines =
                        list
                            |> Maybe.Extra.values
                            |> List.map formatChecksOutput
                            |> List.sortBy
                                (\row ->
                                    case List.Extra.getAt 3 row of
                                        Just "Different outputs" ->
                                            0

                                        Just "Download failed" ->
                                            1

                                        Just "No elm-explorations/test in deps" ->
                                            2

                                        Just "No tests" ->
                                            3

                                        Just "Pass" ->
                                            4

                                        _ ->
                                            -1
                                )

                    summaryHeader : List ( String, Maybe Float )
                    summaryHeader =
                        [ ( "Result", Just 27.59 )
                        , ( "Count", Just 16 )
                        ]

                    summaryLines : List (List String)
                    summaryLines =
                        resultLines
                            |> List.filterMap (List.Extra.getAt 3)
                            |> Dict.Extra.groupBy identity
                            |> CoreDict.map (\k l -> [ k, String.fromInt (List.length l) ])
                            |> CoreDict.values

                    summaryFooter : List (List String)
                    summaryFooter =
                        [ [ "Total", String.fromInt (List.length resultLines) ] ]
                in
                [ ( "Results"
                  , Xlsx.gridToSheetWithColumnWidths
                        (List.map Tuple.second resultsHeader)
                        (List.map Tuple.first resultsHeader :: resultLines)
                  )
                , ( "Summary"
                  , Xlsx.gridToSheetWithColumnWidths
                        (List.map Tuple.second summaryHeader)
                        (List.map Tuple.first summaryHeader
                            :: summaryLines
                            ++ summaryFooter
                        )
                  )
                ]
                    |> Xlsx.writeWorkbook
            )


getTools : String -> BuildTask () FatalError Tools
getTools pwd =
    BuildTask.succeed Tools
        |> BuildTask.andMap (BuildTask.which (pwd ++ "/node_modules/.bin/elm-test"))
        |> BuildTask.andMap (BuildTask.which "elm-test-rs")
        |> BuildTask.andMap (BuildTask.which "pigz")
        |> BuildTask.andMap (BuildTask.which "tar")
        |> BuildTask.andMap
            (allCompilers
                |> List.map BuildTask.which
                |> BuildTask.combine
                |> BuildTask.map
                    (List.foldl
                        (\command acc -> Dict.insert command.name command acc)
                        Dict.empty
                    )
            )


formatChecksOutput : ( Package, CheckResult ) -> List String
formatChecksOutput ( package, checkResult ) =
    let
        specific : List String
        specific =
            case checkResult of
                CompilerOutputs outputs ->
                    let
                        msg : String
                        msg =
                            case Set.size (Set.fromList (Dict.values outputs)) of
                                0 ->
                                    "Internal error - no outputs"

                                1 ->
                                    "Pass"

                                _ ->
                                    "Different outputs"
                    in
                    allCompilers
                        |> List.map
                            (\compiler ->
                                Dict.get compiler outputs
                                    |> Maybe.withDefault ""
                                    |> String.Extra.ellipsis 1000
                            )
                        |> (::) msg

                NoTests ->
                    [ "No tests" ]

                DownloadFailed reason ->
                    [ "Download failed", reason ]

                MissingElmExplorationsDependency ->
                    [ "No elm-explorations/test in deps" ]
    in
    [ package.author
    , package.name
    , package.version
    ]
        ++ specific


handlePackage : { pwd : String } -> Package -> BuildTask Tools FatalError CheckResult
handlePackage pwd package =
    BuildTask.do (downloadPackage package) <| \downloadResult ->
    case downloadResult of
        Err e ->
            BuildTask.succeed (DownloadFailed e)

        Ok { downloaded, hasTests } ->
            let
                packageString : String
                packageString =
                    package.author ++ "/" ++ package.name
            in
            if not hasTests then
                BuildTask.succeed NoTests

            else
                Do.allowFatal (BuildTask.readFromDirectory downloaded "elm.json") <| \elmJsonString ->
                case Json.Decode.decodeString Project.decoder elmJsonString of
                    Err e ->
                        Json.Decode.errorToString e
                            |> FatalError.fromString
                            |> BuildTask.fail

                    Ok (Project.Application _) ->
                        "Unexpected application-style elm.json"
                            |> FatalError.fromString
                            |> BuildTask.fail

                    Ok (Project.Package elmJson) ->
                        runTestsForPackage pwd elmJson downloaded


type ElmTestVersion
    = ElmTestV1
    | ElmTestV2


type CheckResult
    = CompilerOutputs (Dict String String)
    | NoTests
    | DownloadFailed String
    | MissingElmExplorationsDependency


runTestsForPackage : { pwd : String } -> Project.PackageInfo -> FileOrDirectory -> BuildTask Tools FatalError CheckResult
runTestsForPackage pwd elmJson downloaded =
    let
        elmTestDependency : Maybe ( Package.Name, Constraint )
        elmTestDependency =
            (elmJson.testDeps ++ elmJson.deps)
                |> List.Extra.find
                    (\( name, _ ) -> Just name == Package.fromString "elm-explorations/test")
    in
    case elmTestDependency of
        Nothing ->
            BuildTask.succeed MissingElmExplorationsDependency

        Just ( _, elmTestConstraint ) ->
            let
                check : String -> Bool
                check versionString =
                    case Version.fromString versionString of
                        Nothing ->
                            False

                        Just version ->
                            Constraint.check version elmTestConstraint
            in
            if List.any check [ "1.0.0", "1.1.0", "1.2.0", "1.2.1", "1.2.2" ] then
                innerRunTestsForPackage pwd elmJson downloaded ElmTestV1

            else if List.any check [ "2.0.0", "2.0.1", "2.1.0", "2.1.1", "2.1.2", "2.2.0", "2.2.1" ] then
                innerRunTestsForPackage pwd elmJson downloaded ElmTestV2

            else
                ("Unrecognized elm-explorations/test version: " ++ Constraint.toString elmTestConstraint)
                    |> FatalError.fromString
                    |> BuildTask.fail


compilerVersions : ElmTestVersion -> List String
compilerVersions elmTestVersion =
    case elmTestVersion of
        ElmTestV1 ->
            [ "elm-0.19.1"
            , "lamdera-1.3.2-no-wire"
            , "lamdera-1.4.0-no-wire"
            , "lamdera-next-no-wire"
            ]

        ElmTestV2 ->
            allCompilers


allCompilers : List String
allCompilers =
    [ "elm-0.19.1"
    , "elm-0.19.2"
    , "lamdera-1.3.2-no-wire"
    , "lamdera-1.4.0-no-wire"
    , "lamdera-next-no-wire"
    ]


innerRunTestsForPackage :
    { pwd : String }
    -> Project.PackageInfo
    -> FileOrDirectory
    -> ElmTestVersion
    -> BuildTask Tools FatalError CheckResult
innerRunTestsForPackage { pwd } elmJson downloaded elmTestVersion =
    let
        elmTestPath : Tools -> Command
        elmTestPath =
            case elmTestVersion of
                ElmTestV1 ->
                    .elm_test

                ElmTestV2 ->
                    .elm_test_rs

        elmTestArgs : String -> List String
        elmTestArgs compiler =
            [ "--report"
            , "json"
            , "--seed"
            , Hash.toString downloaded
                |> FNV1a.hash
                |> String.fromInt
            , "--compiler"
            , compiler
            ]

        compilerOutputsTask : BuildTask Tools FatalError (List ( String, String ))
        compilerOutputsTask =
            BuildTask.do (BuildTask.getTool identity) <| \tools ->
            compilerVersions elmTestVersion
                |> List.map
                    (\compiler ->
                        BuildTask.Unsafe.commandInWritableDirectoryOutputWith
                            (CommandOptions.default
                                |> CommandOptions.withOutput Stream.MergeStderrAndStdout
                                |> CommandOptions.allowNon0Status
                                |> CommandOptions.withTimeout Duration.minute
                            )
                            elmTestPath
                            (elmTestArgs compiler)
                            downloaded
                            |> BuildTask.withEnv [ ( "ELM_HOME", pwd ++ "/elm-homes/" ++ compiler ) ]
                            |> BuildTask.withMemoryLimitInMB 1500
                            |> BuildTask.withIdlePriority
                            -- |> BuildTask.withDebug Debug.todo
                            |> BuildTask.mapError
                                (\e ->
                                    case e.recoverable of
                                        Stream.StreamError internal ->
                                            "Command failed with an internal stream error: " ++ internal

                                        Stream.CustomError _ body ->
                                            Maybe.withDefault "<no body>" body
                                )
                            |> BuildTask.andThen
                                (\r ->
                                    if String.contains "\"duration\"" r then
                                        r
                                            |> String.lines
                                            |> List.Extra.removeWhen String.isEmpty
                                            |> List.Extra.last
                                            |> Maybe.withDefault r
                                            |> replaceDuration
                                            |> BuildTask.succeed

                                    else
                                        formatError r
                                            |> BuildTask.succeed
                                )
                            |> BuildTask.mapError
                                (\e ->
                                    FatalError.build
                                        { title = "Test failed"
                                        , body =
                                            [ "Testing of " ++ Package.toString elmJson.name ++ " failed for " ++ compiler
                                            , "Command was:\n  "
                                                ++ String.join " " ((elmTestPath tools).name :: elmTestArgs compiler)
                                            , e
                                            , "--  " ++ Package.toString elmJson.name ++ ", " ++ compiler
                                            ]
                                                |> String.join "\n\n"
                                        }
                                )
                            |> BuildTask.map
                                (\r ->
                                    ( compiler
                                    , normalizeCompilerOutput r
                                    )
                                )
                    )
                |> BuildTask.combine
    in
    BuildTask.do compilerOutputsTask <| \compilerOutputs ->
    BuildTask.succeed (CompilerOutputs (Dict.fromList compilerOutputs))


pathRegex : Regex
pathRegex =
    Regex.fromString "/[^ ]*/workspace-[0-9a-f]*/"
        |> Maybe.withDefault Regex.never


normalizeCompilerOutput : String -> String
normalizeCompilerOutput r =
    r
        |> Regex.replace pathRegex (\_ -> "")
        |> simplify404Error
        |> String.replace "0.19.2" "0.19.1"
        |> String.replace "lamdera" "elm"
        |> String.trim


simplify404Error : String -> String
simplify404Error r =
    let
        -- Lamdera changes this error message a bit, but we don't care
        key : String
        key =
            "But it came back as 404 Not Found"
    in
    case String.indexes key r of
        [ i ] ->
            String.left (i + String.length key) r

        _ ->
            r


formatError : String -> String
formatError s =
    if String.contains "`elm make` failed with exit code 1." s then
        s

    else
        case Json.Decode.decodeString ErrorParser.elmTestErrorDecoder s of
            Err e ->
                s ++ "\nAdditionally, formatting failed because:\n" ++ Json.Decode.errorToString e

            Ok parsed ->
                ErrorParser.formatElmTestError parsed


replaceDuration : String -> String
replaceDuration s =
    Regex.replace durationRegex (\_ -> "\"duration\": \"---\"") s


durationRegex : Regex
durationRegex =
    Regex.fromString "\"duration\": *\"[0-9]+(\\.[0-9]+)?\""
        |> Maybe.withDefault Regex.never


formatJson : String -> String
formatJson f =
    let
        isPrintable : Char -> Bool
        isPrintable c =
            let
                code : Int
                code =
                    Char.toCode c
            in
            -- Crude heuristic
            0x20 <= code && code < 0x7F
    in
    case Json.Decode.decodeString Json.Decode.value f of
        Err _ ->
            if String.all isPrintable f then
                f

            else
                f
                    |> Bytes.Encode.string
                    |> Bytes.Encode.encode
                    |> Hex.Convert.toString
                    |> Hex.Convert.blocks 2
                    |> List.Extra.greedyGroupsOf 0x10
                    |> List.indexedMap
                        (\rowIndex row ->
                            let
                                rowString : String
                                rowString =
                                    String.padLeft 8 '0' (Hex.fromWord32 (rowIndex * 0x10))

                                hexes : String
                                hexes =
                                    row
                                        |> List.Extra.greedyGroupsOf 8
                                        |> List.map (String.join " ")
                                        |> String.join "  "

                                maybePrintable : String
                                maybePrintable =
                                    row
                                        |> List.map
                                            (\hex ->
                                                hex
                                                    |> XBytes.fromHex
                                                    |> Maybe.andThen XBytes.toText
                                                    |> Maybe.andThen String.uncons
                                                    |> Maybe.map Tuple.first
                                                    |> Maybe.Extra.filter isPrintable
                                                    |> Maybe.withDefault '.'
                                            )
                                        |> String.fromList
                            in
                            [ rowString
                            , String.padRight (16 * 3) ' ' hexes
                            , "|" ++ maybePrintable ++ "|"
                            ]
                                |> String.join "  "
                        )
                    |> String.join "\n"

        Ok v ->
            Json.Encode.encode 2 v


packageListDecoder : Json.Decode.Decoder (List Package)
packageListDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                case packageFromString raw of
                    Err e ->
                        Json.Decode.fail e

                    Ok o ->
                        Json.Decode.succeed o
            )
        |> Json.Decode.list


packageFromString : String -> Result String { author : String, name : String, version : String }
packageFromString raw =
    case String.split "@" raw of
        [ before, version ] ->
            case String.split "/" before of
                [ author, name ] ->
                    Ok
                        { author = author
                        , name = name
                        , version = version
                        }

                _ ->
                    Err ("Could not split package author and name in " ++ escape raw)

        _ ->
            Err ("Could not split package author/name and version in " ++ escape raw)


downloadPackage : Package -> BuildTask Tools FatalError (Result String { downloaded : FileOrDirectory, hasTests : Bool })
downloadPackage package =
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

        getRoot : List String -> BuildTask Tools FatalError String
        getRoot contents =
            case contents of
                firstLine :: _ ->
                    case String.split "/" firstLine of
                        [ root, "" ] ->
                            BuildTask.succeed root

                        _ ->
                            ("Unexpected first content line: " ++ escape firstLine)
                                |> FatalError.fromString
                                |> BuildTask.fail

                [] ->
                    -- Empty file, root is irrelevant
                    BuildTask.succeed ""
    in
    Do.allowFatal (BuildTask.Unsafe.downloadImmutable url) <| \tarGz ->
    Do.allowFatal (BuildTask.Gzip.gunzip tarGz) <| \tar ->
    BuildTask.do (BuildTask.Tar.listContents tar) <| \contents ->
    BuildTask.do (getRoot contents) <| \root ->
    let
        toExtract : Result String (List String)
        toExtract =
            contents
                |> Result.Extra.combineMap
                    (\file ->
                        if String.isEmpty file || String.endsWith "/" file then
                            Ok Nothing

                        else if not (String.startsWith (root ++ "/") file) then
                            Err ("Unexpected file " ++ escape file ++ ", expected root " ++ root)

                        else
                            let
                                cut : String
                                cut =
                                    String.dropLeft (String.length root + 1) file
                            in
                            if
                                (cut == "elm.json")
                                    || (String.endsWith ".elm" cut
                                            && (String.startsWith "src/" cut || String.startsWith "tests/" cut)
                                       )
                            then
                                Ok (Just cut)

                            else
                                Ok Nothing
                    )
                |> Result.map Maybe.Extra.values
    in
    case toExtract of
        Err e ->
            BuildTask.fail (FatalError.fromString e)

        Ok list ->
            if List.Extra.notMember "elm.json" list then
                BuildTask.succeed (Err "Missing elm.json")

            else
                let
                    hasTests : Bool
                    hasTests =
                        List.any
                            (\f ->
                                String.startsWith (root ++ "/tests/") f
                                    && String.endsWith ".elm" f
                            )
                            contents
                in
                BuildTask.do (BuildTask.Tar.extract { stripPrefix = Just root } tar list) <| \result ->
                BuildTask.succeed (Ok { downloaded = result, hasTests = hasTests })


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ "@" ++ package.version
