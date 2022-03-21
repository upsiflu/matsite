module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withBody, withOrientation, withoutCaption, withAdditionalAttributes
    , view
    , orientationToString
    )

{-| contain the immutable site content

_To render Segments differently based on their position in the tree, use
[`Segment.Viewmode`](Accordion.Segment.ViewMode)_

@docs Segment
@docs empty, singleton
@docs Orientation


# Map

@docs withBody, withOrientation, withoutCaption, withAdditionalAttributes


# View

@docs view

-}

import Accordion.Segment.ViewMode as ViewMode exposing (Role(..), ViewMode(..))
import Css exposing (..)
import Fold exposing (Direction(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Layout exposing (..)
import Zipper.Tree as Tree exposing (Tree)


{-| -}
type alias Segment msg =
    { caption : Maybe String
    , id : String
    , body : Maybe (Html msg)
    , orientation : Orientation
    , columnCount : Int
    , additionalAttributes : List (Html.Attribute Never)
    }


{-| -}
type Orientation
    = Vertical
    | Horizontal


{-| -}
withOrientation : Orientation -> Segment msg -> Segment msg
withOrientation orientation segment =
    { segment | orientation = orientation }


{-| -}
withBody : Html msg -> Segment msg -> Segment msg
withBody body segment =
    { segment | body = Just body }


{-| -}
withoutCaption : Segment msg -> Segment msg
withoutCaption segment =
    { segment | caption = Nothing }


{-| -}
withAdditionalAttributes : List (Html.Attribute Never) -> Segment msg -> Segment msg
withAdditionalAttributes cc segment =
    { segment | additionalAttributes = cc ++ segment.additionalAttributes }


{-| -}
singleton : String -> Segment msg
singleton id =
    { caption = Just id
    , id = String.replace " " "-" id
    , body = Nothing
    , orientation = Vertical
    , columnCount = 1
    , additionalAttributes = []
    }


{-| -}
empty : Segment msg
empty =
    { caption = Nothing
    , id = "_"
    , body = Nothing
    , orientation = Vertical
    , columnCount = 1
    , additionalAttributes = []
    }



---- View and helpers ----


{-| -}
view : ViewMode -> Segment msg -> Html msg
view mode s =
    let
        defaultLayout =
            if
                s.orientation
                    == Horizontal
                    && s.body
                    == Nothing
            then
                [ id segmentId, css [ overflowY scroll, Css.maxWidth (rem 4), position relative, Css.property "writing-mode" "vertical-rl" ], ViewMode.toClass mode ]

            else
                [ id segmentId, css [ overflowY scroll, Css.width (rem 21), position relative ], ViewMode.toClass mode ]

        viewOrientation =
            Html.div [ css [ position absolute, left zero, top zero ] ]
                [ Html.text (orientationToString s.orientation) ]

        ----
        viewOverlay =
            Html.text
                >> List.singleton
                >> Html.div
                    [ css [ position absolute, right zero, top zero, color (rgb 255 255 0), backgroundColor (rgba 255 255 0 0.1), Css.property "writing-mode" "horizontal-tb" ] ]

        segmentId =
            if mode == Placeholder then
                ""

            else
                s.id

        viewCaption =
            Maybe.withDefault "𐫱"
                >> header "" segmentId

        viewBody =
            Maybe.withDefault (Html.text "")

        highlight =
            case ViewMode.role mode of
                Parent ->
                    css [ backgroundColor (rgb 10 10 255) ]

                Aisle ->
                    css [ backgroundColor (rgba 255 255 255 0.1) ]

                Focus ->
                    css [ backgroundColor (rgba 0 0 255 0.5) ]

                _ ->
                    css []

        additionalAttributes =
            css [ Css.property "--column-count" (String.fromInt s.columnCount)]
                :: s.additionalAttributes
                |> List.map (Attributes.map never)

        structureClass =
            classList [ ( "noCaption", s.caption == Nothing ), ( "hasBody", s.body /= Nothing ) ]
    in
    case mode of
        Default path ->
            Html.div (ViewMode.toClass mode :: class "default" :: class (orientationToString s.orientation) :: id segmentId :: structureClass :: additionalAttributes)
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection path |> String.join "")
                , viewBody s.body
                , viewOrientation
                , ViewMode.view mode
                ]

        Collapsed path ->
            Html.div (ViewMode.toClass mode :: class "collapsed" :: class (orientationToString s.orientation) :: id segmentId :: structureClass :: additionalAttributes)
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection path |> String.join "")
                , viewBody s.body
                , viewOrientation
                , ViewMode.view mode
                ]

        Placeholder ->
            Html.div (ViewMode.toClass mode :: class "placeholder" :: class (orientationToString s.orientation) :: id segmentId :: structureClass :: additionalAttributes)
                [ viewCaption s.caption
                , viewBody s.body
                , ViewMode.view mode
                ]


{-| -}
orientationToString orientation =
    case orientation of
        Horizontal ->
            "🀱"

        Vertical ->
            "🁣"
