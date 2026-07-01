module CompilerTestBuildfile exposing (Package, buildAction, getInput)

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Tar
import BuildTask.Unsafe
import BuildTask.Unsafe.Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Maybe.Extra
import Path
import Url.Builder


type alias Package =
    { author : String
    , name : String
    , version : String
    }


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
                            ++ "] "
                            ++ packageToString package
                            ++ " "
                in
                downloadPackage package
                    |> BuildTask.withPrefix prefix
                    |> BuildTask.map
                        (Maybe.map
                            (\( downloaded, hasTests ) ->
                                { filename = Path.path (String.join "/" [ package.author, package.name, package.version ])
                                , hash = downloaded
                                }
                            )
                        )
            )
        |> BuildTask.combine
        |> BuildTask.andThen
            (\list ->
                list
                    |> Maybe.Extra.values
                    |> BuildTask.combineInto
            )


getInput : BackendTask FatalError (List Package)
getInput =
    -- 0.19 cutoff
    Http.get "https://package.elm-lang.org/all-packages/since/6557"
        (Http.expectJson packageListDecoder)
        |> BackendTask.allowFatal


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


downloadPackage : Package -> BuildTask (Maybe ( FileOrDirectory, Bool ))
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
    (BuildTask.Unsafe.Do.downloadImmutable url <| \tarGz ->
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
    )
        |> BuildTask.toResult
        |> BuildTask.map Result.toMaybe


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ ":" ++ package.version
