module ErrorParser exposing (CompileError, ElmTestError(..), ErrorMessageFragment(..), Position, Problem, elmTestErrorDecoder, formatElmTestError)

import Ansi.Color
import Ansi.Font
import Json.Decode
import Json.Encode


type ElmTestError
    = Error
        { path : Maybe String
        , title : String
        , message : List ErrorMessageFragment
        }
    | CompileErrors (List CompileError)


type ErrorMessageFragment
    = Text String
    | Formatted
        { bold : Bool
        , underline : Bool
        , color : Maybe Ansi.Color.Color
        , string : String
        }


type alias CompileError =
    { path : String
    , name : String
    , problems : List Problem
    }


type alias Problem =
    { title : String
    , region :
        { start : Position
        , end : Position
        }
    , message : List ErrorMessageFragment
    }


type alias Position =
    { line : Int
    , column : Int
    }


elmTestErrorDecoder : Json.Decode.Decoder ElmTestError
elmTestErrorDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Error errorDecoder
        , Json.Decode.map CompileErrors compileErrorsDecoder
        ]


compileErrorsDecoder : Json.Decode.Decoder (List CompileError)
compileErrorsDecoder =
    Json.Decode.map2 always
        (Json.Decode.field "errors" (Json.Decode.list compileErrorDecoder))
        (Json.Decode.field "type" (constDecoder Json.Decode.string Json.Encode.string "compile-errors" ()))


compileErrorDecoder : Json.Decode.Decoder CompileError
compileErrorDecoder =
    Json.Decode.map3
        CompileError
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "problems" (Json.Decode.list decodeProblem))


decodeProblem : Json.Decode.Decoder Problem
decodeProblem =
    Json.Decode.map3
        Problem
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field
            "region"
            (Json.Decode.map2
                (\start end -> { start = start, end = end })
                (Json.Decode.field "start" decodePosition)
                (Json.Decode.field "end" decodePosition)
            )
        )
        (Json.Decode.field "message" (Json.Decode.list messageFragment))


decodePosition : Json.Decode.Decoder Position
decodePosition =
    Json.Decode.map2 Position
        (Json.Decode.field "line" Json.Decode.int)
        (Json.Decode.field "column" Json.Decode.int)


errorDecoder : Json.Decode.Decoder { path : Maybe String, title : String, message : List ErrorMessageFragment }
errorDecoder =
    Json.Decode.map4 (\() path title message -> { path = path, title = title, message = message })
        (Json.Decode.field "type" (constDecoder Json.Decode.string Json.Encode.string "error" ()))
        (Json.Decode.field "path" (Json.Decode.nullable Json.Decode.string))
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "message" (Json.Decode.list messageFragment))


messageFragment : Json.Decode.Decoder ErrorMessageFragment
messageFragment =
    Json.Decode.oneOf
        [ Json.Decode.map Text Json.Decode.string
        , Json.Decode.map4
            (\bold underline color string ->
                Formatted
                    { bold = bold
                    , underline = underline
                    , color = color
                    , string = string
                    }
            )
            (Json.Decode.field "bold" Json.Decode.bool)
            (Json.Decode.field "underline" Json.Decode.bool)
            (Json.Decode.field "color"
                (Json.Decode.oneOf
                    [ constDecoder Json.Decode.string Json.Encode.string "GREEN" (Just Ansi.Color.Green)
                    , constDecoder Json.Decode.string Json.Encode.string "RED" (Just Ansi.Color.Red)
                    , constDecoder Json.Decode.string Json.Encode.string "yellow" (Just Ansi.Color.Yellow)
                    , Json.Decode.null Nothing
                    ]
                )
            )
            (Json.Decode.field "string" Json.Decode.string)
        ]


constDecoder : Json.Decode.Decoder a -> (a -> Json.Encode.Value) -> a -> v -> Json.Decode.Decoder v
constDecoder dec enc exp val =
    dec
        |> Json.Decode.andThen
            (\r ->
                if r == exp then
                    Json.Decode.succeed val

                else
                    Json.Decode.fail ("Expected " ++ Json.Encode.encode 0 (enc exp))
            )


formatElmTestError : ElmTestError -> String
formatElmTestError parsed =
    case parsed of
        Error { path, title, message } ->
            [ Ansi.Color.fontColor Ansi.Color.cyan ("-- " ++ title ++ " --------------- " ++ Maybe.withDefault "" path)
            , formatErrorMessage message
            ]
                |> String.join "\n"

        CompileErrors errors ->
            errors
                |> List.map formatCompileError
                |> String.join "\n\n"


formatCompileError : CompileError -> String
formatCompileError error =
    ("# " ++ Ansi.Color.fontColor Ansi.Color.cyan error.name ++ " -- " ++ error.path)
        :: List.map formatCompilerProblem error.problems
        |> String.join "\n\n"


formatCompilerProblem : Problem -> String
formatCompilerProblem problem =
    "## " ++ problem.title ++ "\n" ++ formatErrorMessage problem.message


formatErrorMessage : List ErrorMessageFragment -> String
formatErrorMessage fragment =
    fragment
        |> List.map formatErrorMessageFragment
        |> String.concat


formatErrorMessageFragment : ErrorMessageFragment -> String
formatErrorMessageFragment fragment =
    case fragment of
        Text text ->
            text

        Formatted { bold, underline, color, string } ->
            string
                |> (if bold then
                        Ansi.Font.bold

                    else
                        identity
                   )
                -- |> (if underline then
                --         Ansi.Font.underline
                --     else
                --         identity
                --    )
                |> (case color of
                        Just c ->
                            Ansi.Color.fontColor c

                        Nothing ->
                            identity
                   )
