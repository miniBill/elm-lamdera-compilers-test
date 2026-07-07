module CompilerTestBuildfile exposing (Inputs, Package, buildAction, getInput)

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
import Diff
import Diff.ToString
import Elm.Constraint
import Elm.Package
import Elm.Project
import Elm.Version as Version
import FatalError exposing (FatalError)
import Hash
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Pages.Script as Script
import Path exposing (Path)
import Regex exposing (Regex)
import Result.Extra
import Url.Builder


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
                        |> BuildTask.withPrefix prefix
            )
        |> BuildTask.combine
        |> BuildTask.andThen
            (\list ->
                list
                    |> Maybe.Extra.values
                    |> BuildTask.combineInto
            )
        |> BuildTask.andThen (\_ -> BuildTask.writeFile "TODO" |> BuildTask.allowFatal)


handlePackage : Package -> BuildTask FatalError (Maybe { filename : Path, hash : FileOrDirectory })
handlePackage package =
    BuildTask.do (downloadPackage package) <| \downloadResult ->
    case downloadResult of
        Err e ->
            BuildTask.succeed Nothing
                |> BuildTask.withWarning e

        Ok ( downloaded, hasTests ) ->
            if not hasTests then
                { filename = Path.path (String.join "/" [ package.author, package.name, package.version ])
                , hash = downloaded
                }
                    |> Just
                    |> BuildTask.succeed

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


runTestsForPackage : Elm.Project.PackageInfo -> FileOrDirectory -> BuildTask FatalError (Maybe { filename : Path, hash : FileOrDirectory })
runTestsForPackage elmJson downloaded =
    BuildTask.do pwdTask <| \pwd ->
    let
        elmTestPathTask : BuildTask FatalError (Maybe String)
        elmTestPathTask =
            case List.Extra.find (\( name, _ ) -> Just name == Elm.Package.fromString "elm-explorations/test") elmJson.testDeps of
                Nothing ->
                    BuildTask.succeed Nothing
                        |> BuildTask.withWarning
                            ("Could not find an elm-explorations/test dependency:\n"
                                ++ Json.Encode.encode 2
                                    (Elm.Project.encode (Elm.Project.Package elmJson))
                            )

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
                        (pwd ++ "/node_modules/.bin/elm-test")
                            |> Just
                            |> BuildTask.succeed

                    else if List.any check [ "2.0.0", "2.0.1", "2.1.0", "2.1.1", "2.1.2", "2.2.0", "2.2.1" ] then
                        "elm-test-rs"
                            |> Just
                            |> BuildTask.succeed

                    else
                        ("Unrecognized elm-explorations/test version: " ++ Elm.Constraint.toString elmTestConstraint)
                            |> FatalError.fromString
                            |> BuildTask.fail
    in
    BuildTask.do elmTestPathTask <| \elmTestPathMaybe ->
    case elmTestPathMaybe of
        Nothing ->
            BuildTask.succeed Nothing

        Just elmTestPath ->
            let
                compilerOutputsTask : BuildTask FatalError (List ( String, String ))
                compilerOutputsTask =
                    [ "elm-0.19.1"

                    -- , "elm-0.19.2"
                    , "lamdera-1.3.2"
                    , "lamdera-1.4.0"
                    ]
                        |> List.map
                            (\compiler ->
                                BuildTask.Unsafe.commandInWritableDirectoryOutputWith
                                    (Stream.defaultCommandOptions
                                        |> Stream.allowNon0Status
                                    )
                                    elmTestPath
                                    [ "--report"
                                    , "json"
                                    , "--seed"
                                    , "123456789"
                                    , "--compiler"
                                    , compiler
                                    ]
                                    downloaded
                                    |> BuildTask.withEnv [ ( "ELM_HOME", pwd ++ "/elm-homes/" ++ compiler ) ]
                                    |> BuildTask.map
                                        (\r ->
                                            ( compiler
                                            , r
                                                |> String.lines
                                                |> List.Extra.removeWhen String.isEmpty
                                                |> List.Extra.last
                                                |> Maybe.withDefault r
                                                |> replaceDuration
                                            )
                                        )
                                    |> BuildTask.mapError
                                        (\e ->
                                            FatalError.build
                                                { title =
                                                    "Compilation of "
                                                        ++ Elm.Package.toString elmJson.name
                                                        ++ " failed for "
                                                        ++ compiler
                                                , body =
                                                    case e.recoverable of
                                                        Stream.StreamError internal ->
                                                            "Internal error: " ++ internal

                                                        Stream.CustomError _ body ->
                                                            Maybe.withDefault "<no body>" body
                                                }
                                        )
                            )
                        |> BuildTask.combine
            in
            BuildTask.do compilerOutputsTask <| \compilerOutputs ->
            case List.Extra.uniqueBy Tuple.second compilerOutputs of
                [] ->
                    "No compiler outputs"
                        |> FatalError.fromString
                        |> BuildTask.fail

                [ _ ] ->
                    { filename =
                        Path.path
                            (String.join "/"
                                [ Elm.Package.toString elmJson.name
                                , Version.toString elmJson.version
                                ]
                            )
                    , hash = downloaded
                    }
                        |> Just
                        |> BuildTask.succeed

                ( c1, r1 ) :: ( c2, r2 ) :: _ ->
                    let
                        body : String
                        body =
                            Diff.diffLinesWith Diff.defaultOptions
                                (formatJson r1)
                                (formatJson r2)
                                |> Diff.ToString.diffToString { context = 3, color = True }
                    in
                    FatalError.build
                        { title =
                            Elm.Package.toString elmJson.name
                                ++ "@"
                                ++ Version.toString elmJson.version
                                ++ " Difference between "
                                ++ c1
                                ++ " and "
                                ++ c2
                        , body = body
                        }
                        |> BuildTask.fail


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
    case Json.Decode.decodeString Json.Decode.value f of
        Err _ ->
            f

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


downloadPackage : Package -> BuildTask FatalError (Result String ( FileOrDirectory, Bool ))
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
                BuildTask.succeed (Ok ( result, hasTests ))


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ "@" ++ package.version
