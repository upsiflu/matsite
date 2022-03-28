module Accordion.Segment.ViewMode exposing
    ( ViewMode
    , path
    , toClass
    , Offset, Region(..), Width(..), addWidth, cumulativeOffset, offsetToCssVariables, regionToString, toCssVariables, zeroOffset
    )

{-| reflects a Segment's position within the Tree

![Accordion Structure](../asset/22-03-17-Accordion.svg)


## To Do

  - [x] Push the `Role` type into `Tree` and name it `Position`

  - [x] Add a `Width` type

        type Width
            = Columns Int
            | Screen

  - [x] Add a `Region` type

        type Region
            = North
            | South
            | West
            | East
            | NearWest
            | NearEast
            | Here

  - [x] Remove `Placeholder` and `Collapsed`; instead, use

        { position = Tree.Position, region = Region, offset = List Width }

---

@docs ViewMode, Role


# Deconstruct

@docs role, path


# View

@docs view, toClass

-}

import Css exposing (..)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html
import Html.Styled.Attributes exposing (class, css)
import Layout
import List.Extra as List


{-| -}
type alias ViewMode =
    { position : Position, region : Region, offset : Offset }


{-| -}
type Region
    = North
    | South
    | West
    | East
    | NearWest
    | NearEast
    | Center
    | Peek
    | Cache


{-| -}
type Width
    = Columns Int
    | Screen


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

        Peek ->
            "peek"

        Cache ->
            "cache"


viewWidth : Width -> String
viewWidth width =
    case width of
        Columns c ->
            String.fromInt c ++ "-column"

        Screen ->
            "screen"


type alias Offset =
    { screens : Int, columns : Int, units : Int }


zeroOffset : Offset
zeroOffset =
    { screens = 0, columns = 0, units = 0 }


{-| -}
cumulativeOffset : List ViewMode -> Offset
cumulativeOffset =
    let
        addOffset : Offset -> Offset -> Offset
        addOffset { screens, columns, units } acc =
            { screens = screens + acc.screens, columns = columns + acc.columns, units = units + acc.units }
    in
    zeroOffset
        |> List.foldl (.offset >> addOffset)


{-| -}
addWidth : Width -> Offset -> Offset
addWidth w acc =
    case w of
        Columns c ->
            { acc | columns = acc.columns + c, units = acc.units + 1 }

        Screen ->
            { acc | screens = acc.screens + 1, units = acc.units + 1 }


toString : ViewMode -> String
toString mode =
    let
        pos =
            Fold.viewPosition mode.position

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
offsetToCssVariables { screens, columns, units } =
    [ ( "screens", screens )
    , ( "columns", columns )
    , ( "units", units )
    ]
