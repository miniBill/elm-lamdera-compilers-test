module Main exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script as Script exposing (Script, makeDirectory)
import Set
import Time
import Tui
import Tui.Effect as Effect exposing (Effect)
import Tui.Screen as Screen exposing (Screen)
import Tui.Sub
import Url.Builder


type Model
    = GettingPackageList
    | DownloadingPackages
        { done : List ( Package, Bool )
        , doing : List Package
        , todo : List Package
        , failed : List Package
        }
    | FatalError String


type Msg
    = GotPackageList (Result Http.Error (List Package))
    | DownloadedPackage Package (Result DownloadError Bool)
    | Tick Time.Posix


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


inFlightDownloads : Int
inFlightDownloads =
    10


run : Script
run =
    Tui.program
        { data = BackendTask.succeed ()
        , init = \() -> ( GettingPackageList, getPackageList )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
        |> Tui.toScript


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotPackageList (Err e) ->
            ( FatalError (Debug.toString e), Effect.none )

        GotPackageList (Ok packages) ->
            ( DownloadingPackages
                { todo =
                    packages
                        |> List.Extra.removeWhen
                            (\package ->
                                -- 404
                                package.author == "quietgarden"
                            )
                        |> List.sortBy packageToString
                , doing = []
                , done = []
                , failed = []
                }
            , Effect.none
            )

        DownloadedPackage package (Err e) ->
            case model of
                DownloadingPackages inner ->
                    ( DownloadingPackages
                        { doing = List.Extra.remove package inner.doing
                        , done = inner.done
                        , todo = inner.todo
                        , failed = package :: inner.failed
                        }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        DownloadedPackage package (Ok hasTests) ->
            case model of
                DownloadingPackages inner ->
                    ( DownloadingPackages
                        { doing = List.Extra.remove package inner.doing
                        , done = ( package, hasTests ) :: inner.done
                        , todo = inner.todo
                        , failed = inner.failed
                        }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        Tick _ ->
            case model of
                DownloadingPackages inner ->
                    inner |> fillQueue

                _ ->
                    ( model, Effect.none )


fillQueue :
    { doing : List Package
    , done : List ( Package, Bool )
    , todo : List Package
    , failed : List Package
    }
    -> ( Model, Effect Msg )
fillQueue ({ todo, doing, done, failed } as data) =
    let
        toAddCount : Int
        toAddCount =
            inFlightDownloads - List.length doing
    in
    if toAddCount == 0 then
        ( DownloadingPackages data, Effect.none )

    else
        let
            ( toAdd, newTodo ) =
                List.Extra.splitAt toAddCount todo
        in
        ( DownloadingPackages
            { todo = newTodo
            , doing = doing ++ toAdd
            , done = done
            , failed = failed
            }
        , toAdd
            |> List.map (\package -> Effect.attempt (DownloadedPackage package) (downloadPackage package))
            |> Effect.batch
        )


getPackageList : Effect Msg
getPackageList =
    -- 0.19 cutoff
    Http.get "https://package.elm-lang.org/all-packages/since/6557"
        (Http.expectJson packageListDecoder)
        |> BackendTask.mapError .recoverable
        |> Effect.attempt GotPackageList


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
                                        "Could not split package author and name in " ++ Json.Encode.encode 0 (Json.Encode.string raw)
                                in
                                Json.Decode.fail msg

                    _ ->
                        let
                            msg : String
                            msg =
                                "Could not split package author/name and version in " ++ Json.Encode.encode 0 (Json.Encode.string raw)
                        in
                        Json.Decode.fail msg
            )
        |> Json.Decode.list


downloadPackage : Package -> BackendTask DownloadError Bool
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
    in
    Do.do (File.exists targetFolder) <| \exists ->
    Do.do
        (if exists then
            Do.noop

         else
            doDownloadPackage package
        )
    <| \_ ->
    File.exists (targetFolder ++ "/test")


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
                        Json.Encode.encode 0 (Json.Encode.string s)

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


view : Tui.Context -> Model -> Screen
view { width, height, colorProfile } model =
    case model of
        FatalError err ->
            Screen.fg Ansi.Color.red (Screen.text err)

        GettingPackageList ->
            Screen.text (icons.running ++ " Getting package list")

        DownloadingPackages { todo, doing, done } ->
            let
                toLines : String -> List Package -> List Screen
                toLines label list =
                    list
                        |> List.Extra.gatherEqualsBy (\p -> [ p.author, p.name ])
                        |> List.sortBy (\( h, _ ) -> packageToString h)
                        |> List.map (\( h, t ) -> { author = h.author, name = h.name, versions = h.version :: List.map .version t })
                        |> List.map (\l -> Screen.text (label ++ " " ++ packagesToString l))

                minDoneHeight : Int
                minDoneHeight =
                    4

                doingLines : Int
                doingLines =
                    min (height - 1 - minDoneHeight) (List.length doing)

                ( todoColumn, doneColumn ) =
                    mapLonger Tuple.pair
                        (toLines icons.waiting todo)
                        (toLines icons.done (List.map Tuple.first done))
                        |> List.take (height - 1 - doingLines)
                        |> List.unzip
                        |> Tuple.mapBoth
                            (\c ->
                                c
                                    |> List.filterMap identity
                                    |> Screen.lines
                            )
                            (\c ->
                                c
                                    |> List.filterMap identity
                                    |> Screen.lines
                            )

                totalCount : Int
                totalCount =
                    List.length todo + List.length doing + List.length done
            in
            (Screen.text ("Downloading " ++ String.fromInt totalCount ++ " packages")
                :: toLines icons.running (List.take doingLines doing)
                ++ [ Screen.concat [ todoColumn, doneColumn ] ]
            )
                |> List.take height
                |> Screen.lines


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


icons :
    { done : String
    , waiting : String
    , running : String
    }
icons =
    { done = "✅"
    , waiting = "🕑"
    , running = "🏃"
    }


packageToString : Package -> String
packageToString package =
    package.author ++ "/" ++ package.name ++ ":" ++ package.version


subscriptions : Model -> Tui.Sub.Sub Msg
subscriptions model =
    Tui.Sub.everyMillis 100 Tick
