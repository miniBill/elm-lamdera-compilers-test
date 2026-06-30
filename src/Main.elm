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


type DownloadError
    = FailedToRemoveDirectory String
    | FailedToRemoveFile String
    | FailedToMakeDirectory String
    | FailedToExecute String (List String)
    | FailedToStatFile String
    | DownloadedEmptyFile String String


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
            { todo =
                packages
                    |> List.Extra.removeWhen
                        (\package ->
                            List.member package.author
                                [ "Skinney"
                                , "pdandy"
                                , "ryanhg"
                                ]
                        )
            , doing = []
            , done = []
            , failed = []
            }
                |> fillQueue

        DownloadedPackage package (Err e) ->
            case model of
                DownloadingPackages inner ->
                    { doing = List.Extra.remove package inner.doing
                    , done = inner.done
                    , todo = inner.todo
                    , failed = package :: inner.failed
                    }
                        |> fillQueue

                _ ->
                    ( model, Effect.none )

        DownloadedPackage package (Ok hasTests) ->
            case model of
                DownloadingPackages inner ->
                    { doing = List.Extra.remove package inner.doing
                    , done = ( package, hasTests ) :: inner.done
                    , todo = inner.todo
                    , failed = inner.failed
                    }
                        |> fillQueue

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
    Http.get "https://package.elm-lang.org/search.json"
        (Http.expectJson packageListDecoder)
        |> BackendTask.mapError .recoverable
        |> Effect.attempt GotPackageList


packageListDecoder : Json.Decode.Decoder (List Package)
packageListDecoder =
    Json.Decode.list
        (Json.Decode.map2
            (\( author, name ) version ->
                { author = author
                , name = name
                , version = version
                }
            )
            (Json.Decode.field "name"
                (Json.Decode.string
                    |> Json.Decode.andThen
                        (\raw ->
                            case String.split "/" raw of
                                [ author, name ] ->
                                    Json.Decode.succeed ( author, name )

                                _ ->
                                    let
                                        msg : String
                                        msg =
                                            "Could not split package author and name in " ++ Json.Encode.encode 0 (Json.Encode.string raw)
                                    in
                                    Json.Decode.fail msg
                        )
                )
            )
            (Json.Decode.field "version" Json.Decode.string)
        )


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

        filename : String
        filename =
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

        tarArgs : List String -> List String
        tarArgs files =
            [ "xzf"
            , filename
            , "--strip-components"
            , "1"
            , "-C"
            , tmpFolder
            ]
                ++ List.map
                    (\file -> package.name ++ "-" ++ package.version ++ "/" ++ file)
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
            Script.exec cmd args
                |> BackendTask.mapError (\_ -> FailedToExecute cmd args)

        stat : String -> BackendTask DownloadError Glob.FileStats
        stat file =
            Glob.succeed identity
                |> Glob.match (Glob.literal file)
                |> Glob.captureStats
                |> Glob.expectUniqueMatch
                |> BackendTask.mapError (\_ -> FailedToStatFile file)
    in
    Do.do (removeDirectoryRecursive tmpFolder) <| \_ ->
    Do.do (removeFile filename) <| \_ ->
    Do.do (makeDirectoryRecursive tmpFolder) <| \_ ->
    Do.do (exec "curl" [ "-sSL", url, "--remove-on-error", "-o", filename ]) <| \_ ->
    Do.do (stat filename) <| \filenameStats ->
    if filenameStats.sizeInBytes == 0 then
        BackendTask.fail (DownloadedEmptyFile url filename)

    else
        Do.do (makeDirectoryRecursive tmpFolder) <| \_ ->
        Do.do (exec "tar" (tarArgs [ "elm.json", "src" ])) <| \_ ->
        Do.do
            (exec "tar" (tarArgs [ "tests" ])
                -- Ignore errors here
                |> BackendTask.toResult
            )
        <| \_ ->
        Do.do (makeDirectoryRecursive targetFolderContainer) <| \_ ->
        Do.do (exec "mv" [ tmpFolder, targetFolder ]) <| \_ ->
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
                        |> List.sortBy packageToString
                        |> List.map (\l -> Screen.text (label ++ " " ++ packageToString l))

                toLinesDone : String -> List ( Package, Bool ) -> List Screen
                toLinesDone label list =
                    list
                        |> List.sortBy (\( package, _ ) -> packageToString package)
                        |> List.map
                            (\( package, hasTests ) ->
                                Screen.text
                                    (label
                                        ++ " "
                                        ++ packageToString package
                                        ++ " "
                                        ++ (if hasTests then
                                                "has tests"

                                            else
                                                "no tests"
                                           )
                                    )
                            )

                minDoneHeight : Int
                minDoneHeight =
                    4

                doingLines : Int
                doingLines =
                    min (height - 1 - minDoneHeight) (List.length doing)

                ( todoColumn, doneColumn ) =
                    mapLonger Tuple.pair
                        (toLines icons.waiting todo)
                        (toLinesDone icons.done done)
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
subscriptions _ =
    Tui.Sub.none
