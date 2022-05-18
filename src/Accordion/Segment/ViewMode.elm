module Accordion.Segment.ViewMode exposing
    ( defaultPeek
    , ViewMode
    , Region(..)
    , regionToString
    , Width(..), addWidth, widthToString, Offset, cumulativeOffset
    , offsetToCssVariables, zeroOffset
    , path
    , toCssVariables
    , toClass
    , widthCodec
    )

{-| reflects a Segment's position within the Tree

![Accordion Structure](../asset/22-03-17-Accordion.svg)


# Create

@docs defaultPeek

---

@docs ViewMode

---

@docs Region
@docs regionToString


### Width

@docs Width, addWidth, widthToString, Offset, cumulativeOffset
@docs offsetToCssVariables, zeroOffset


# Deconstruct

@docs path
@docs toCssVariables


# View

@docs toClass

-}

import Codec exposing (Codec, bool, field, float, int, maybeField, string, variant0, variant1, variant2)
import Css exposing (..)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html
import Html.Styled.Attributes exposing (class, css)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout
import List.Extra as List
import Time


{-| -}
type alias ViewMode =
    { zone : Maybe ( String, Time.Zone ), position : Position, region : Region, offset : Offset }


{-| -}
type Region
    = North
    | South
    | West
    | East
    | NearWest
    | NearEast
    | Center
    | Peek { targetId : String, hint : String }
    | Cache


{-| -}
defaultPeek : Region
defaultPeek =
    Peek { targetId = "", hint = "" }


{-| -}
type Width
    = Columns Int
    | Screen


{-| -}
widthCodec : Codec Width
widthCodec =
    Codec.custom
        (\col scr value ->
            case value of
                Columns i ->
                    col i

                Screen ->
                    scr
        )
        |> Codec.variant1 "Columns" Columns Codec.int
        |> Codec.variant0 "Screen" Screen
        |> Codec.buildCustom


{-| -}
path : ViewMode -> List Direction
path =
    .position >> .path



---- View ----


{-| -}
regionToString : Region -> String
regionToString region =
    case region of
        North ->
            "north"

        South ->
            "south"

        West ->
            "west"

        East ->
            "east"

        NearWest ->
            "nearWest"

        NearEast ->
            "nearEast"

        Center ->
            "center"

        Peek _ ->
            "peek"

        Cache ->
            "cache"


{-| -}
widthToString : Width -> String
widthToString width =
    case width of
        Columns c ->
            String.fromInt c ++ "-column"

        Screen ->
            "screen"


{-| encode the cumulative shift from the origin in a series of Segments
-}
type alias Offset =
    { screens : Int, columns : Int, units : Int, headers : Int, infoLines : Int }


{-| -}
zeroOffset : Offset
zeroOffset =
    { screens = 0, columns = 0, units = 0, headers = 0, infoLines = 0 }


{-| -}
cumulativeOffset : List ViewMode -> Offset
cumulativeOffset =
    let
        addOffset : Offset -> Offset -> Offset
        addOffset { screens, columns, units, headers, infoLines } acc =
            { screens = screens + acc.screens, columns = columns + acc.columns, units = units + acc.units, headers = headers + acc.headers, infoLines = infoLines + acc.infoLines }
    in
    zeroOffset
        |> List.foldl (.offset >> addOffset)


isPeek : ViewMode -> Bool
isPeek { region } =
    case region of
        Peek _ ->
            True

        _ ->
            False


isParent : ViewMode -> Bool
isParent =
    path >> (==) [ Up ]


{-| -}
addWidth : ViewMode -> Bool -> Width -> Int -> Offset -> Offset
addWidth mode isExpanded width infoLineCount acc =
    let
        respectInfoLines : Bool
        respectInfoLines =
            mode.region == Center || isPeek mode || isParent mode

        infoLines =
            if respectInfoLines then
                infoLineCount

            else
                0
    in
    case ( isExpanded, width ) of
        ( True, Columns c ) ->
            { acc | columns = acc.columns + c, units = acc.units + 1 + infoLines, infoLines = acc.infoLines + infoLines }

        ( True, Screen ) ->
            { acc | screens = acc.screens + 1, units = acc.units + 1 + infoLines, infoLines = acc.infoLines + infoLines }

        ( False, _ ) ->
            { acc | headers = acc.headers + 1, units = acc.units + 1 + infoLines, infoLines = acc.infoLines + infoLines }


toString : ViewMode -> String
toString mode =
    let
        pos =
            Fold.positionToString mode.position

        reg =
            regionToString mode.region

        off =
            mode.offset
                |> (\o ->
                        "🏛️" ++ String.fromInt o.columns ++ " 💻" ++ String.fromInt o.screens ++ " 🟡" ++ String.fromInt o.units
                   )
    in
    [ pos, reg, off ]
        |> String.join " "


{-| -}
toClass : ViewMode -> Html.Attribute msg
toClass =
    toString >> class


{-| -}
toCssVariables : ViewMode -> Html.Attribute msg
toCssVariables =
    .offset >> offsetToCssVariables >> List.map Layout.toProperty >> css


{-| -}
offsetToCssVariables : Offset -> List ( String, Int )
offsetToCssVariables { screens, columns, units, headers, infoLines } =
    [ ( "screens", screens )
    , ( "columns", columns )
    , ( "units", units )
    , ( "headers", headers )
    , ( "infoLines", infoLines )
    ]
