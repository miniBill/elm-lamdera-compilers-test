module Xlsx exposing (gridToSheet, writeWorkbook)

import BuildTask exposing (BuildTask, FileOrDirectory)
import Bytes.Encode
import Dict as CoreDict
import Dict.Extra
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import List.Extra
import Regex exposing (Regex)
import String.Extra
import Time
import Xml.Encode
import Zip exposing (Zip)
import Zip.Entry


type alias Workbook =
    List ( String, Sheet )


type alias Sheet =
    Dict ( Int, Int ) Cell


type alias Cell =
    String


gridToSheet : List (List String) -> Sheet
gridToSheet cells =
    cells
        |> List.indexedMap (\rowIndex row -> row |> List.indexedMap (\colIndex cell -> ( ( rowIndex, colIndex ), cell )))
        |> List.concat
        |> Dict.fromList


writeWorkbook : Workbook -> BuildTask FatalError FileOrDirectory
writeWorkbook workbook =
    workbook
        |> workbookToZip
        |> Zip.toBytes
        |> BuildTask.writeBytes


workbookToZip : Workbook -> Zip
workbookToZip workbook =
    workbook
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( sheetIndex, ( sheetName, sheet ) ) acc ->
                Zip.insert (sheetToEntry sheetIndex sheetName sheet) acc
            )
            (writeMetadata workbook)


writeMetadata : Workbook -> Zip
writeMetadata workbook =
    Zip.empty
        |> Zip.insert (contentTypesXml (List.length workbook))
        |> Zip.insert dotRels
        |> Zip.insert (workbookXml workbook)
        |> Zip.insert (workbookXmlRels (List.length workbook))
        |> Zip.insert style


tag : String -> List ( String, String ) -> List Xml.Encode.Value -> Xml.Encode.Value
tag name attrs children =
    Xml.Encode.Tag name
        (attrs
            |> List.map (\( k, v ) -> ( k, Xml.Encode.string v ))
            |> CoreDict.fromList
        )
        (Xml.Encode.Object children)


contentTypesXml : Int -> Zip.Entry.Entry
contentTypesXml size =
    [ tag "Types"
        [ ( "xmlns", "http://schemas.openxmlformats.org/package/2006/content-types" ) ]
        ([ tag "Default"
            [ ( "Extension", "bin" )
            , ( "ContentType", "application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings" )
            ]
            []
         , tag "Default"
            [ ( "Extension", "rels" )
            , ( "ContentType", "application/vnd.openxmlformats-package.relationships+xml" )
            ]
            []
         , tag "Default"
            [ ( "Extension", "xml" )
            , ( "ContentType", "application/xml" )
            ]
            []
         , tag "Override"
            [ ( "PartName", "/xl/workbook.xml" )
            , ( "ContentType", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml" )
            ]
            []
         , tag "Override"
            [ ( "PartName", "/xl/styles.xml" )
            , ( "ContentType", "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml" )
            ]
            []
         ]
            ++ List.map
                (\i ->
                    tag "Override"
                        [ ( "PartName", "/xl/worksheets/sheet" ++ String.fromInt i ++ ".xml" )
                        , ( "ContentType", "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml" )
                        ]
                        []
                )
                (List.range 1 size)
        )
    ]
        |> xmlEntry "[Content_Types].xml"


dotRels : Zip.Entry.Entry
dotRels =
    [ tag "Relationships"
        [ ( "xmlns", "http://schemas.openxmlformats.org/package/2006/relationships" ) ]
        [ tag "Relationship"
            [ ( "Id", "rId1" )
            , ( "Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" )
            , ( "Target", "xl/workbook.xml" )
            ]
            []
        ]
    ]
        |> xmlEntry "_rels/.rels"


workbookXml : Workbook -> Zip.Entry.Entry
workbookXml workbook =
    [ tag "workbook"
        [ ( "xmlns", "http://schemas.openxmlformats.org/spreadsheetml/2006/main" )
        , ( "xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships" )
        ]
        [ tag "workbookPr" [ ( "defaultThemeVersion", "124226" ) ] []
        , tag "bookViews"
            []
            [ tag "workbookView" [ ( "activeTab", "0" ) ] []
            ]
        , tag "sheets"
            []
            (workbook
                |> List.indexedMap
                    (\i ( name, _ ) ->
                        tag "sheet"
                            [ ( "name", name )
                            , ( "sheetId", String.fromInt (i + 1) )
                            , ( "r:id", "rId" ++ String.fromInt (i + 2) )
                            ]
                            []
                    )
            )
        , tag "calcPr"
            [ ( "calcId", "124519" )
            , ( "fullCalcOnLoad", "1" )
            ]
            []
        ]
    ]
        |> xmlEntry "xl/workbook.xml"


workbookXmlRels : Int -> Zip.Entry.Entry
workbookXmlRels size =
    [ tag "Relationships"
        [ ( "xmlns", "http://schemas.openxmlformats.org/package/2006/relationships" ) ]
        -- [tag "Relationship"
        --   [ ( "Id", "rId1" )
        --   , ( "Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" )
        --   , ( "Target", "styles.xml" )
        --   ]
        --   []]++
        (List.map
            (\i ->
                tag "Relationship"
                    [ ( "Id", "rId" ++ String.fromInt (i + 1) )
                    , ( "Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" )
                    , ( "Target", "worksheets/sheet" ++ String.fromInt i ++ ".xml" )
                    ]
                    []
            )
            (List.range 1 size)
        )
    ]
        |> xmlEntry "xl/_rels/workbook.xml.rels"


style : Zip.Entry.Entry
style =
    [ tag "styleSheet"
        [ ( "xmlns", "http://schemas.openxmlformats.org/spreadsheetml/2006/main" ) ]
        [ tag "numFmts" [] []

        --  "    <numFmts count=\"1\">"
        --  "        <!-- based from openoffice output -->"
        --  "        <numFmt numFmtId=\"164\" formatCode=\"General\"/>"
        -- + "    </numFmts>"
        , tag "fonts" [] []

        -- + "    <fonts count=\"4\">"
        -- + "        <!-- based from https://github.com/mk-j/PHP_XLSXWriter/blob/master/xlsxwriter.class.php#L472 and openoffice output -->"
        -- + "        <font>"
        -- + "            <sz val=\"10\"/>"
        -- + "            <name val=\"Arial\"/>"
        -- + "            <family val=\"2\"/>"
        -- + "        </font>"
        -- + "        <font>"
        -- + "            <sz val=\"10\"/>"
        -- + "            <name val=\"Arial\"/>"
        -- + "            <family val=\"0\"/>"
        -- + "        </font>"
        -- + "        <font>"
        -- + "            <sz val=\"10\"/>"
        -- + "            <name val=\"Arial\"/>"
        -- + "            <family val=\"0\"/>"
        -- + "        </font>"
        -- + "        <font>"
        -- + "            <sz val=\"10\"/>"
        -- + "            <name val=\"Arial\"/>"
        -- + "            <family val=\"0\"/>"
        -- + "        </font>"
        -- + "        <!-- hardcoded test -->"
        -- + "        <!-- bold --><!--<font>"
        -- + "            <b val=\"true\"/>"
        -- + "            <sz val=\"10\"/>"
        -- + "            <name val=\"Arial\"/>"
        -- + "            <family val=\"2\"/>"
        -- + "        </font>-->"
        -- + "        <!-- italic --><!--<font>"
        -- + "            <i val=\"true\"/>"
        -- + "            <sz val=\"10\"/>"
        -- + "            <name val=\"Arial\"/>"
        -- + "            <family val=\"2\"/>"
        -- + "        </font>-->"
        -- + "        <!-- -->"
        -- + "    </fonts>"
        , tag "fills" [] []

        -- + "    <fills count=\"2\">"
        -- + "        <!-- based from openoffice output -->"
        -- + "        <fill><patternFill patternType=\"none\"/></fill>"
        -- + "        <fill><patternFill patternType=\"gray125\"/></fill>"
        -- + "    </fills>"
        , tag "borders" [] []

        -- + "    <borders count=\"1\">"
        -- + "        <!-- based from openoffice output -->"
        -- + "        <border diagonalDown=\"false\" diagonalUp=\"false\"><left/><right/><top/><bottom/><diagonal/></border>"
        -- + "    </borders>"
        , tag "cellStyleXfs" [] []

        -- + "    <cellStyleXfs count=\"20\"> <!-- based from openoffice output -->"
        -- + "        <xf numFmtId=\"164\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"true\" applyAlignment=\"true\" applyProtection=\"true\">"
        -- + "            <alignment horizontal=\"general\" vertical=\"bottom\" textRotation=\"0\" wrapText=\"false\" indent=\"0\" shrinkToFit=\"false\"/>"
        -- + "            <protection locked=\"true\" hidden=\"false\"/>"
        -- + "        </xf>"
        -- + "        <xf numFmtId=\"0\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"2\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"2\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"43\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"41\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"44\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"42\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "        <xf numFmtId=\"9\" fontId=\"1\" fillId=\"0\" borderId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\"/>"
        -- + "    </cellStyleXfs>"
        , tag "cellXfs" [] []

        -- + "    <cellXfs count=\"1\">"
        -- + "        <!-- based from openoffice output -->"
        -- + "        <!-- default -->"
        -- + "        <xf numFmtId=\"164\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"false\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\">"
        -- + "            <alignment horizontal=\"general\" vertical=\"bottom\" textRotation=\"0\" wrapText=\"false\" indent=\"0\" shrinkToFit=\"false\"/>"
        -- + "            <protection locked=\"true\" hidden=\"false\"/>"
        -- + "        </xf>"
        -- + "        <!-- hardcoded test -->"
        -- + "        <!-- bold --><!--"
        -- + "        <xf numFmtId=\"164\" fontId=\"4\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\">"
        -- + "            <alignment horizontal=\"general\" vertical=\"bottom\" textRotation=\"0\" wrapText=\"false\" indent=\"0\" shrinkToFit=\"false\"/>"
        -- + "            <protection locked=\"true\" hidden=\"false\"/>"
        -- + "        </xf>-->"
        -- + "        <!-- italic --><!--"
        -- + "        <xf numFmtId=\"164\" fontId=\"5\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"true\" applyBorder=\"false\" applyAlignment=\"false\" applyProtection=\"false\">"
        -- + "            <alignment horizontal=\"general\" vertical=\"bottom\" textRotation=\"0\" wrapText=\"false\" indent=\"0\" shrinkToFit=\"false\"/>"
        -- + "            <protection locked=\"true\" hidden=\"false\"/>"
        -- + "        </xf>-->"
        -- + "        <!-- -->"
        -- + "    </cellXfs>"
        , tag "cellStyles" [] []

        -- + "    <cellStyles>"
        -- + "        <cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\" customBuiltin=\"false\"/>"
        -- + "        <cellStyle name=\"Comma\" xfId=\"15\" builtinId=\"3\" customBuiltin=\"false\"/>"
        -- + "        <cellStyle name=\"Comma [0]\" xfId=\"16\" builtinId=\"6\" customBuiltin=\"false\"/>"
        -- + "        <cellStyle name=\"Currency\" xfId=\"17\" builtinId=\"4\" customBuiltin=\"false\"/>"
        -- + "        <cellStyle name=\"Currency [0]\" xfId=\"18\" builtinId=\"7\" customBuiltin=\"false\"/>"
        -- + "        <cellStyle name=\"Percent\" xfId=\"19\" builtinId=\"5\" customBuiltin=\"false\"/>"
        -- + "    </cellStyles>"
        , tag "dxfs" [] []
        , tag "tableStyles" [] []
        ]
    ]
        |> xmlEntry "xl/styles.xml"


sheetToEntry : Int -> String -> Sheet -> Zip.Entry.Entry
sheetToEntry sheetIndex sheetName sheet =
    [ tag "worksheet"
        [ ( "xmlns", "http://schemas.openxmlformats.org/spreadsheetml/2006/main" ) ]
        [ tag "sheetViews"
            []
            [ tag "sheetView"
                [ ( "workbookViewId", "0" )
                , ( "tabSelected", "true" )
                ]
                []
            ]

        -- , let
        --     colCount : Int
        --     colCount =
        --         sheet
        --             |> Dict.keys
        --             |> List.Extra.maximumBy Tuple.second
        --             |> Maybe.map Tuple.second
        --             |> Maybe.withDefault 0
        --   in
        --   List.range 1 colCount
        --     |> List.map
        --         (\i ->
        --             tag "col"
        --                 [ ( "min", String.fromInt i )
        --                 , ( "max", String.fromInt i )
        --                 ]
        --                 []
        --         )
        --     |> tag "cols" []
        , sheet
            |> Dict.toList
            |> Dict.Extra.groupBy (\( ( r, _ ), _ ) -> r)
            |> CoreDict.toList
            |> List.map rowToXml
            |> tag "sheetData" []
        ]
    ]
        |> xmlEntry ("xl/worksheets/sheet" ++ String.fromInt (sheetIndex + 1) ++ ".xml")


rowToXml : ( Int, List ( ( Int, Int ), String ) ) -> Xml.Encode.Value
rowToXml ( rowIndex, row ) =
    row
        |> List.map
            (\( ( _, colIndex ), cell ) ->
                cellToXml rowIndex colIndex cell
            )
        |> tag "row" [ ( "r", String.fromInt (rowIndex + 1) ) ]


cellToXml : Int -> Int -> String -> Xml.Encode.Value
cellToXml rowIndex colIndex cell =
    cell
        |> String.Extra.ellipsis 200
        |> Xml.Encode.string
        |> List.singleton
        |> tag "t" []
        |> List.singleton
        |> tag "is" []
        |> List.singleton
        |> tag "c"
            [ ( "t", "inlineStr" )
            , ( "r", toReference rowIndex colIndex )
            ]


toReference : Int -> Int -> String
toReference rowIndex colIndex =
    let
        col : Int -> List Char -> String
        col i acc =
            let
                here : Char
                here =
                    Char.fromCode (Char.toCode 'A' + modBy 26 i)
            in
            if i < 0 then
                String.fromList acc

            else
                col (i // 26 - 1) (here :: acc)
    in
    col colIndex [] ++ String.fromInt (rowIndex + 1)


xmlEntry : String -> List Xml.Encode.Value -> Zip.Entry.Entry
xmlEntry path xml =
    ("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        ++ (xml
                |> Xml.Encode.list
                |> xmlToString
           )
    )
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Zip.Entry.store
            { path = path
            , lastModified = ( Time.utc, Time.millisToPosix 0 )
            , comment = Nothing
            }


xmlToString : Xml.Encode.Value -> String
xmlToString value =
    case value of
        Xml.Encode.Tag name props nextValue ->
            "<"
                ++ name
                ++ propsToString props
                ++ ">"
                ++ xmlToString nextValue
                ++ "</"
                ++ name
                ++ ">"

        Xml.Encode.StrNode str ->
            encodeXmlEntities str

        Xml.Encode.IntNode n ->
            String.fromInt n

        Xml.Encode.FloatNode n ->
            String.fromFloat n

        Xml.Encode.BoolNode True ->
            "true"

        Xml.Encode.BoolNode False ->
            "false"

        Xml.Encode.NullNode ->
            ""

        Xml.Encode.Object xs ->
            List.map xmlToString xs
                |> String.join "\n"

        Xml.Encode.DocType name props ->
            "<?"
                ++ name
                ++ propsToString props
                ++ "?>"

        Xml.Encode.CdataNode str ->
            "<![CDATA[" ++ escapeCdataContent str ++ "]]>"


needsEscaping : Regex
needsEscaping =
    Regex.fromString "[&\"'<>]"
        |> Maybe.withDefault Regex.never


needsReplacing : Regex
needsReplacing =
    Regex.fromString "[^\\x09\\x0a\\x0d\\x7f\\u0020-\\ud7ff\\ue000-\\ufffd]"
        |> Maybe.withDefault Regex.never


encodeXmlEntities : String -> String
encodeXmlEntities s =
    s
        |> Regex.replace needsEscaping
            (\{ match } ->
                case match of
                    "&" ->
                        "&amp;"

                    "\"" ->
                        "&quot;"

                    "'" ->
                        "&apos;"

                    "<" ->
                        "&lt;"

                    ">" ->
                        "&gt;"

                    _ ->
                        -- Shouldn't happen
                        toEntity match
            )
        |> Regex.replace needsReplacing
            (\{ match } ->
                -- These characters are never valid in unicode?
                ""
            )


toEntity : String -> String
toEntity match =
    match
        |> String.toList
        |> List.map (\c -> "&#" ++ String.fromInt (Char.toCode c) ++ ";")
        |> String.concat


propsToString : CoreDict.Dict String Xml.Encode.Value -> String
propsToString props =
    if CoreDict.isEmpty props then
        ""

    else
        CoreDict.foldl
            (\key value acc -> acc ++ " " ++ key ++ "=" ++ "\"" ++ propToString value ++ "\"")
            ""
            props


propToString : Xml.Encode.Value -> String
propToString value =
    case value of
        Xml.Encode.StrNode str ->
            encodeXmlEntities str

        Xml.Encode.IntNode n ->
            String.fromInt n

        Xml.Encode.BoolNode True ->
            "true"

        Xml.Encode.BoolNode False ->
            "false"

        Xml.Encode.FloatNode f ->
            String.fromFloat f

        Xml.Encode.CdataNode str ->
            -- CDATA should not be used in attributes, so escape it
            encodeXmlEntities str

        _ ->
            ""


{-| Escape CDATA content by splitting ]]> sequences
-}
escapeCdataContent : String -> String
escapeCdataContent str =
    -- Replace ]]> with ]]]]><![CDATA[> to properly escape it in CDATA
    String.replace "]]>" "]]]]><![CDATA[>" str
