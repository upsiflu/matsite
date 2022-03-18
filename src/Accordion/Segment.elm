module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withBody, withOrientation
    , view
    )

{-| contain the immutable site content

_To render Segments differently based on their position in the tree, use
[`Segment.Viewmode`](Accordion.Segment.ViewMode)_

@docs Segment
@docs empty, singleton
@docs Orientation


# Map

@docs withBody, withOrientation


# View

@docs view

-}

import Accordion.Segment.ViewMode as ViewMode exposing (ViewMode(..))
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
singleton : String -> Segment msg
singleton id =
    { caption = Just id
    , id = String.replace " " "-" id
    , body = Nothing
    , orientation = Vertical
    }


{-| -}
empty : Segment msg
empty =
    { caption = Nothing
    , id = "_"
    , body = Nothing
    , orientation = Vertical
    }



---- View and helpers ----


{-| -}
view : ViewMode -> Segment msg -> Html msg
view mode s =
    let
        default =
            case s.orientation of
                Vertical ->
                    [ css [ overflowY scroll, Css.width (rem 21), position relative ], ViewMode.toClass mode ]

                Horizontal ->
                    [ css [ overflowY scroll, Css.width (rem 4), position relative, Css.property "writing-mode" "vertical-rl" ], ViewMode.toClass mode ]

        imploded =
            [ css [ opacity (num 0.6), maxHeight zero, maxWidth zero, overflow Css.hidden ] ]

        horizontalPlaceholding =
            [ css [ opacity (num 0.8), overflow Css.hidden, maxHeight zero ] ]

        expanded =
            [ id segmentId, css [ maxHeight (calc (vh 100) minus (rem 4)), overflowY scroll, flexGrow (num 1), position relative ] ]

        collapsed =
            [ id segmentId, css [ maxHeight (rem 4), overflow Css.hidden, position relative ] ]

        ----
        viewOverlay =
            Html.text
                >> List.singleton
                >> Html.div
                    [ css [ position absolute, right zero, top zero, color (rgb 255 255 0), backgroundColor (rgb 0 0 100), Css.property "writing-mode" "horizontal-tb" ] ]

        segmentId =
            if ViewMode.isVisible mode then
                s.id

            else
                ""

        viewCaption =
            Maybe.withDefault "𐫱"
                >> header "" segmentId

        viewBody =
            Maybe.withDefault (Html.text "")
    in
    case mode of
        Focus config ->
            Html.div
                (default
                    ++ (if config.expanded then
                            expanded

                        else
                            collapsed
                       )
                )
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection (ViewMode.path mode) |> String.join "")
                , viewBody s.body
                , ViewMode.view mode
                ]

        Spine config ->
            Html.div
                (default
                    ++ (if config.expanded then
                            expanded

                        else
                            collapsed
                       )
                )
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection (ViewMode.path mode) |> String.join "")
                , viewBody s.body
                , ViewMode.view mode
                ]

        Aisle config ->
            Html.div
                (default
                    ++ (if config.expanded then
                            expanded

                        else
                            collapsed
                       )
                )
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection (ViewMode.path mode) |> String.join "")
                , viewBody s.body
                , ViewMode.view mode
                ]

        Periphery config ->
            Html.div
                (default
                    ++ (if not config.visible then
                            imploded

                        else
                            collapsed
                       )
                )
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection (ViewMode.path mode) |> String.join "")
                , viewBody s.body
                , ViewMode.view mode
                ]

        Placeholder ->
            Html.div (default ++ horizontalPlaceholding)
                [ viewCaption s.caption
                , viewOverlay (List.map Fold.viewDirection (ViewMode.path mode) |> String.join "")
                , viewBody s.body
                , ViewMode.view mode
                ]
