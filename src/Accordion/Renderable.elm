module Accordion.Renderable exposing
    ( Renderable
    , singleton
    , nest, nestMany, nestAisle, wrap, div
    , render
    )

{-| Deferred Html Rendering

Accepts a [`ViewMode`](Accordion.Segment.ViewMode) and can be chained with [`nest`](#nest)

@docs Renderable
@docs singleton


# Transform

@docs nest, nestMany, nestAisle, wrap, div


# Unwrap

@docs render

-}

import Accordion.Segment as Segment exposing (Segment)
import Accordion.Segment.ViewMode as ViewSegment
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)


{-| -}
type alias Renderable msg =
    ViewSegment.ViewMode -> Html msg


{-| -}
singleton : Segment msg -> Renderable msg
singleton segment =
    \inheritedMode -> Segment.view inheritedMode segment


{-| apply the `Renderable` to your [`ViewMode`](Accordion.Segment.ViewMode)
-}
render : ViewSegment.ViewMode -> Renderable msg -> Html msg
render =
    (|>)



----- Map ----


{-| -}
nest : ViewSegment.ViewMode -> Renderable msg -> Renderable msg
nest innerMode renderable =
    \inheritedMode -> ViewSegment.contextualize inheritedMode innerMode |> renderable


{-| -}
nestMany : ViewSegment.ViewMode -> List (Renderable msg) -> List (Renderable msg)
nestMany innerMode =
    List.map (nest innerMode)


{-| -}
nestAisle : ViewSegment.ViewMode -> ( List (Renderable msg), List (Renderable msg) ) -> ( List (Renderable msg), List (Renderable msg) )
nestAisle innerMode =
    Tuple.mapBoth (nestMany innerMode) (nestMany innerMode)


{-| -}
wrap : (Html msg -> Html msg) -> Renderable msg -> Renderable msg
wrap wrapper renderable =
    \inheritedMode -> render inheritedMode renderable |> wrapper


{-| -}
div : List (Html.Attribute msg) -> List (Renderable msg) -> Renderable msg
div attr children =
    \inheritedMode ->
        List.map (render inheritedMode) children
            |> Html.div attr
