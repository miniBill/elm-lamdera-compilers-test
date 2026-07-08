module CompilerTestBuildfile exposing (Inputs, Package, buildAction, getInput)

import Ansi.Color
import Ansi.Font
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Gzip
import BuildTask.Internal as Internal
import BuildTask.Tar
import BuildTask.Unsafe
import Bytes.Encode
import CommandOptions
import Diff
import Diff.ToString
import Elm.Constraint
import Elm.Package
import Elm.Project
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
import List.Extra
import Maybe.Extra
import Pages.Script as Script
import Path exposing (Path)
import Regex exposing (Regex)
import Result.Extra
import Url.Builder
import Utils


type alias Package =
    { author : String
    , name : String
    , version : String
    }


type alias Inputs =
    { missing : List Package
    , packages : List Package
    }


getInput : BackendTask FatalError Inputs
getInput =
    BackendTask.map2
        (\missing packages ->
            { missing = missing
            , packages = packages
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


buildAction : Inputs -> BuildTask FatalError FileOrDirectory
buildAction { missing, packages } =
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
                    handlePackage package
                        |> BuildTask.map (\r -> Just ( package, r ))
                        |> BuildTask.withPrefix prefix
            )
        |> BuildTask.combine
        |> BuildTask.andThen
            (\list ->
                list
                    |> Maybe.Extra.values
                    |> List.map formatChecksOutput
                    |> (::) (String.join "\t" [ "Author", "Package", "Version", "Result" ])
                    |> String.join "\n"
                    |> BuildTask.writeFile
                    |> BuildTask.allowFatal
            )


formatChecksOutput : ( Package, CheckResult ) -> String
formatChecksOutput ( package, checkResult ) =
    [ package.author
    , package.name
    , package.version
    , Debug.toString checkResult
    ]
        |> List.map Utils.escape
        |> String.join ";"


handlePackage : Package -> BuildTask FatalError CheckResult
handlePackage package =
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
                case Json.Decode.decodeString Elm.Project.decoder elmJsonString of
                    Err e ->
                        Json.Decode.errorToString e
                            |> FatalError.fromString
                            |> BuildTask.fail

                    Ok (Elm.Project.Application _) ->
                        "Unexpected application-style elm.json"
                            |> FatalError.fromString
                            |> BuildTask.fail

                    Ok (Elm.Project.Package elmJson) ->
                        runTestsForPackage elmJson downloaded


type ElmTestVersion
    = ElmTestV1
    | ElmTestV2


type CheckResult
    = NoCompilerOutputs
    | DifferentOutput { compiler1 : String, compiler2 : String, diff : String }
    | AllOutputsAreTheSame
    | NoTests
    | DownloadFailed String
    | MissingElmExplorationsTestDependency


runTestsForPackage :
    Elm.Project.PackageInfo
    -> FileOrDirectory
    -> BuildTask FatalError CheckResult
runTestsForPackage elmJson downloaded =
    BuildTask.do pwdTask <| \pwd ->
    case List.Extra.find (\( name, _ ) -> Just name == Elm.Package.fromString "elm-explorations/test") elmJson.testDeps of
        Nothing ->
            BuildTask.succeed MissingElmExplorationsTestDependency

        Just ( _, elmTestConstraint ) ->
            let
                check : String -> Bool
                check versionString =
                    case Version.fromString versionString of
                        Nothing ->
                            False

                        Just version ->
                            Elm.Constraint.check version elmTestConstraint
            in
            if List.any check [ "1.0.0", "1.1.0", "1.2.0", "1.2.1", "1.2.2" ] then
                innerRunTestsForPackage elmJson downloaded ElmTestV1

            else if List.any check [ "2.0.0", "2.0.1", "2.1.0", "2.1.1", "2.1.2", "2.2.0", "2.2.1" ] then
                innerRunTestsForPackage elmJson downloaded ElmTestV2

            else
                ("Unrecognized elm-explorations/test version: " ++ Elm.Constraint.toString elmTestConstraint)
                    |> FatalError.fromString
                    |> BuildTask.fail


innerRunTestsForPackage :
    Elm.Project.PackageInfo
    -> FileOrDirectory
    -> ElmTestVersion
    -> BuildTask FatalError CheckResult
innerRunTestsForPackage elmJson downloaded elmTestVersion =
    BuildTask.do pwdTask <| \pwd ->
    let
        elmTestPath : String
        elmTestPath =
            case elmTestVersion of
                ElmTestV1 ->
                    pwd ++ "/node_modules/.bin/elm-test"

                ElmTestV2 ->
                    "elm-test-rs"

        compilerVersions : List String
        compilerVersions =
            case elmTestVersion of
                ElmTestV1 ->
                    [ "elm-0.19.1", "lamdera-1.3.2" ]

                ElmTestV2 ->
                    [ "elm-0.19.1"
                    , "elm-0.19.2"
                    , "lamdera-1.3.2"
                    , "lamdera-1.4.0"
                    ]

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

        compilerOutputsTask : BuildTask FatalError (List ( String, String ))
        compilerOutputsTask =
            compilerVersions
                |> List.map
                    (\compiler ->
                        BuildTask.Unsafe.commandInWritableDirectoryOutputWith
                            (CommandOptions.default
                                |> CommandOptions.withOutput Stream.MergeStderrAndStdout
                                |> CommandOptions.allowNon0Status
                            )
                            elmTestPath
                            (elmTestArgs compiler)
                            downloaded
                            |> BuildTask.withEnv [ ( "ELM_HOME", pwd ++ "/elm-homes/" ++ compiler ) ]
                            |> BuildTask.withMemoryLimitInGB 2
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
                                            [ "Testing of " ++ Elm.Package.toString elmJson.name ++ " failed for " ++ compiler
                                            , "Command was:\n  "
                                                ++ String.join " " (elmTestPath :: elmTestArgs compiler)
                                            , e
                                            , "--  " ++ Elm.Package.toString elmJson.name ++ ", " ++ compiler
                                            ]
                                                |> String.join "\n\n"
                                        }
                                )
                            |> BuildTask.map (\r -> ( compiler, r ))
                    )
                |> BuildTask.combine
    in
    BuildTask.do compilerOutputsTask <| \compilerOutputs ->
    checkCompilerOutputs (Dict.fromList compilerOutputs)
        |> BuildTask.succeed


checkCompilerOutputs : Dict String String -> CheckResult
checkCompilerOutputs compilerOutputs =
    case compilerOutputs |> Dict.toList |> List.Extra.uniqueBy Tuple.second of
        [] ->
            NoCompilerOutputs

        ( compiler1, output1 ) :: ( compiler2, output2 ) :: _ ->
            DifferentOutput
                { compiler1 = compiler1
                , compiler2 = compiler2
                , diff =
                    Diff.diffLinesWith Diff.defaultOptions
                        (formatJson output1)
                        (formatJson output2)
                        |> Diff.ToString.diffToString { context = 3, color = True }
                }

        [ _ ] ->
            AllOutputsAreTheSame


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


pwdTask : BuildTask FatalError String
pwdTask =
    (BuildTask.do (Internal.hashFromString "pwd") <| \outputHash ->
    BuildTask.do
        (Internal.derive "pwd"
            outputHash
            (\{ buildPath } target ->
                Script.command "pwd" []
                    |> BackendTask.andThen
                        (\pwd ->
                            Script.writeFile
                                { path = Hash.toPathTemporary buildPath target
                                , body = pwd
                                }
                                |> BackendTask.allowFatal
                        )
                    |> BackendTask.mapError Internal.InternalError
            )
        )
    <| \pwdFile ->
    BuildTask.withFile pwdFile (\pwd -> BuildTask.succeed (String.trim pwd))
    )
        |> BuildTask.allowFatal


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
                                    String.padLeft 8 '0' (Hex.toString (rowIndex * 0x10))

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
                                                    |> Hex.fromString
                                                    |> Result.toMaybe
                                                    |> Maybe.map Char.fromCode
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


downloadPackage : Package -> BuildTask FatalError (Result String { downloaded : FileOrDirectory, hasTests : Bool })
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

        getRoot : List String -> BuildTask FatalError String
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
