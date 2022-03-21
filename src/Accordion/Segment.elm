module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withBody, withOrientation, withoutCaption, withAdditionalClasses
    , view
    )

{-| contain the immutable site content

_To render Segments differently based on their position in the tree, use
[`Segment.Viewmode`](Accordion.Segment.ViewMode)_

@docs Segment
@docs empty, singleton
@docs Orientation


# Map

@docs withBody, withOrientation, withoutCaption, withAdditionalClasses


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
    , additionalClasses : List String
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
withAdditionalClasses : List String -> Segment msg -> Segment msg
withAdditionalClasses cc segment =
    { segment | additionalClasses = cc ++ segment.additionalClasses }


{-| -}
singleton : String -> Segment msg
singleton id =
    { caption = Just id
    , id = String.replace " " "-" id
    , body = Nothing
    , orientation = Vertical
    , additionalClasses = []
    }


{-| -}
empty : Segment msg
empty =
    { caption = Nothing
    , id = "_"
    , body = Nothing
    , orientation = Vertical
    , additionalClasses = []
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

        collapsedLayout =
            if ViewMode.role mode == Focus then
                if s.orientation == Horizontal then
                    [ id segmentId, css [ overflowY scroll, Css.maxWidth zero, position relative, Css.property "writing-mode" "vertical-rl" ], ViewMode.toClass mode ]

                else
                    [ id segmentId, css [ overflowY scroll, Css.width (rem 21), Css.maxHeight zero, position relative ], ViewMode.toClass mode ]

            else
                defaultLayout

        placeholderLayout =
            if s.orientation == Horizontal then
                [ css [ overflowY scroll, Css.maxWidth zero, position relative, Css.property "writing-mode" "vertical-rl" ], ViewMode.toClass mode ]

            else
                [ css [ overflowY scroll, Css.width (rem 21), Css.maxHeight zero, position relative ], ViewMode.toClass mode ]

        orientationToString =
            case s.orientation of
                Horizontal ->
                    "🀱"

                Vertical ->
                    "🁣"

        viewOrientation =
            Html.div [ css [ position absolute, left zero, top zero ] ]
                [ Html.text orientationToString ]

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

        additionalClasses =
            s.additionalClasses
                |> List.map (\c -> ( c, True ))
                |> classList

        structureClass =
            classList [ ( "noCaption", s.caption == Nothing ), ( "hasBody", s.body /= Nothing ) ]
    in
    case mode of
        Default path ->
            Html.div [ ViewMode.toClass mode, class "default", class orientationToString, id segmentId, structureClass, additionalClasses ]
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection path |> String.join "")
                , viewBody s.body
                , viewOrientation
                , ViewMode.view mode
                ]

        Collapsed path ->
            Html.div [ ViewMode.toClass mode, class "collapsed", class orientationToString, id segmentId, structureClass, additionalClasses ]
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection path |> String.join "")
                , viewBody s.body
                , viewOrientation
                , ViewMode.view mode
                ]

        Placeholder ->
            Html.div [ ViewMode.toClass mode, class "placeholder", class orientationToString, id segmentId, structureClass, additionalClasses ]
                [ viewCaption s.caption
                , viewBody s.body
                , ViewMode.view mode
                ]
