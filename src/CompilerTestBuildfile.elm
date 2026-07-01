module CompilerTestBuildfile exposing (Inputs, Package, buildAction, getInput)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http as Http
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Tar
import BuildTask.Unsafe.Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Maybe.Extra
import Path
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
    BackendTask.map2 (\missing packages -> { missing = missing, packages = packages })
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
        (Http.get "https://package.elm-lang.org/all-packages/since/6557"
            (Http.expectJson packageListDecoder)
            |> BackendTask.allowFatal
        )


buildAction : { missing : List Package, packages : List Package } -> BuildTask FileOrDirectory
buildAction { missing, packages } =
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
                if List.member package missing then
                    BuildTask.succeed (Err (packageToString package))

                else
                    downloadPackage package
                        |> BuildTask.toResult
                        |> BuildTask.andThen
                            (\r ->
                                case r of
                                    Ok ( downloaded, hasTests ) ->
                                        { filename = Path.path (String.join "/" [ package.author, package.name, package.version ])
                                        , hash = downloaded
                                        }
                                            |> Ok
                                            |> BuildTask.succeed

                                    Err e ->
                                        let
                                            _ =
                                                Debug.log prefix e
                                        in
                                        BuildTask.succeed (Err (packageToString package))
                            )
                        |> BuildTask.withPrefix prefix
            )
        |> BuildTask.combine
        |> BuildTask.andThen
            (\list ->
                let
                    ( oks, errs ) =
                        Result.Extra.partition list
                in
                BuildTask.andThen2
                    (\repos newMissing ->
                        BuildTask.combineInto
                            [ { filename = Path.path "repos", hash = repos }
                            , { filename = Path.path "missing", hash = newMissing }
                            ]
                    )
                    (BuildTask.combineInto oks)
                    (BuildTask.writeFile (String.join "\n" errs))
            )


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


downloadPackage : Package -> BuildTask ( FileOrDirectory, Bool )
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
            let
                hasTests : Bool
                hasTests =
                    List.member (root ++ "/tests/") contents
            in
            BuildTask.do (BuildTask.Tar.extract { stripPrefix = Just root } tar list) <| \result ->
            BuildTask.succeed ( result, hasTests )


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ ":" ++ package.version
