module CompilerTestBuildfile exposing (Inputs, Package, buildAction, getInput)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http as Http
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Tar
import BuildTask.Unsafe
import BuildTask.Unsafe.Do
import Diff
import Diff.ToString
import Elm.Constraint
import Elm.Package
import Elm.Project
import FatalError exposing (FatalError)
import Hash
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Path exposing (Path)
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


buildAction : Inputs -> BuildTask FileOrDirectory
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

                    task : BuildTask (Maybe { filename : Path, hash : FileOrDirectory })
                    task =
                        if List.member package missing then
                            BuildTask.succeed Nothing

                        else
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
                                        BuildTask.do
                                            (BuildTask.Unsafe.commandInReadonlyDirectory "pwd" [] downloaded)
                                        <| \pwdFile ->
                                        BuildTask.do (BuildTask.withFile pwdFile BuildTask.succeed) <| \pwdString ->
                                        BuildTask.do
                                            (BuildTask.Unsafe.commandInReadonlyDirectory "cat" [ "elm.json" ] downloaded)
                                        <| \elmJsonFile ->
                                        BuildTask.do (BuildTask.withFile elmJsonFile BuildTask.succeed) <| \elmJsonString ->
                                        case Json.Decode.decodeString Elm.Project.decoder elmJsonString of
                                            Err e ->
                                                BuildTask.fail (Json.Decode.errorToString e)

                                            Ok (Elm.Project.Application _) ->
                                                BuildTask.fail "Unexpected application-style elm.json"

                                            Ok (Elm.Project.Package elmJson) ->
                                                let
                                                    elmTestPathTask : BuildTask (Maybe String)
                                                    elmTestPathTask =
                                                        case List.Extra.find (\( name, _ ) -> Just name == Elm.Package.fromString "elm-explorations/test") elmJson.testDeps of
                                                            Nothing ->
                                                                BuildTask.succeed Nothing
                                                                    |> BuildTask.withWarning ("Could not find an elm-explorations/test dependency in build/" ++ Hash.toString downloaded ++ " - " ++ pwdString ++ "\n" ++ elmJsonString)

                                                            Just ( _, v ) ->
                                                                let
                                                                    constraintString : String
                                                                    constraintString =
                                                                        Elm.Constraint.toString v
                                                                in
                                                                if String.endsWith "v < 2.0.0" constraintString then
                                                                    -- "/home/minibill/src/miniBill/elm-lamdera-compilers-test/node_modules/.bin/elm-test"
                                                                    --     |> Just
                                                                    --     |> BuildTask.succeed
                                                                    BuildTask.succeed Nothing
                                                                        |> BuildTask.withWarning "elm-test 1 not supported (yet - PRs welcome)"

                                                                else if String.endsWith "v < 3.0.0" constraintString then
                                                                    "/home/minibill/src/miniBill/elm-lamdera-compilers-test/node_modules/.bin/elm-test-rs"
                                                                        |> Just
                                                                        |> BuildTask.succeed

                                                                else
                                                                    BuildTask.fail ("Unrecognized elm-explorations/test version: " ++ constraintString)

                                                    commonElmTestOptions : List String
                                                    commonElmTestOptions =
                                                        [ "--report"
                                                        , "json"
                                                        , "--seed"
                                                        , "123456789"
                                                        ]
                                                in
                                                BuildTask.do elmTestPathTask <| \elmTestPathMaybe ->
                                                case elmTestPathMaybe of
                                                    Nothing ->
                                                        BuildTask.succeed Nothing

                                                    Just elmTestPath ->
                                                        let
                                                            compilerOutputsTask =
                                                                [ "elm-0.19.1", "lamdera-1.3.2", "lamdera-1.4.0" ]
                                                                    |> List.map
                                                                        (\compiler ->
                                                                            BuildTask.Unsafe.commandInWritableDirectoryOutput elmTestPath
                                                                                (commonElmTestOptions ++ [ "--compiler", compiler ])
                                                                                downloaded
                                                                                |> BuildTask.withEnv [ ( "ELM_HOME", "./elm-home-for-" ++ compiler ) ]
                                                                                |> BuildTask.map (\r -> ( compiler, r ))
                                                                        )
                                                                    |> BuildTask.combine
                                                        in
                                                        BuildTask.do compilerOutputsTask <| \compilerOutputs ->
                                                        case List.Extra.uniqueBy Tuple.second compilerOutputs of
                                                            [] ->
                                                                BuildTask.fail "No compiler outputs"

                                                            [ _ ] ->
                                                                { filename = Path.path (String.join "/" [ package.author, package.name, package.version ])
                                                                , hash = downloaded
                                                                }
                                                                    |> Just
                                                                    |> BuildTask.succeed

                                                            ( c1, r1 ) :: ( c2, r2 ) :: _ ->
                                                                Diff.diffLinesWith Diff.defaultOptions
                                                                    (formatJson r1)
                                                                    (formatJson r2)
                                                                    |> Diff.ToString.diffToString { context = 3, color = True }
                                                                    |> (++) (c1 ++ " vs " ++ c2)
                                                                    |> BuildTask.fail
                in
                task
                    |> BuildTask.toResult
                    |> BuildTask.andThen
                        (\v ->
                            case v of
                                Ok o ->
                                    BuildTask.succeed o

                                Err e ->
                                    BuildTask.fail (Debug.toString e)
                        )
                    |> BuildTask.withPrefix prefix
            )
        |> BuildTask.combine
        |> BuildTask.andThen
            (\list ->
                list
                    |> Maybe.Extra.values
                    |> BuildTask.combineInto
            )


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


downloadPackage : Package -> BuildTask (Result String ( FileOrDirectory, Bool ))
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

        getRoot : List String -> BuildTask String
        getRoot contents =
            case contents of
                firstLine :: _ ->
                    case String.split "/" firstLine of
                        [ root, "" ] ->
                            BuildTask.succeed root

                        _ ->
                            BuildTask.fail ("Unexpected first content line: " ++ escape firstLine)

                [] ->
                    -- Empty file, root is irrelevant
                    BuildTask.succeed ""
    in
    BuildTask.Unsafe.Do.downloadImmutable url <| \tarGz ->
    BuildTask.Unsafe.Do.pipeThrough "gunzip" [] tarGz <| \tar ->
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
            BuildTask.fail e

        Ok list ->
            if List.Extra.notMember "elm.json" list then
                BuildTask.succeed (Err "Missing elm.json")

            else
                let
                    hasTests : Bool
                    hasTests =
                        List.member (root ++ "/tests/") contents
                in
                BuildTask.do (BuildTask.Tar.extract { stripPrefix = Just root } tar list) <| \result ->
                BuildTask.succeed (Ok ( result, hasTests ))


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ "@" ++ package.version
