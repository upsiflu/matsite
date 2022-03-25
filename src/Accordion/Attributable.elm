module Accordion.Attributable exposing
    ( Att
    , create
    , Renderer, AcceptsAttributes
    , withAttributes
    , view, viewWith
    )

{-| Builder that allows for adding attributes _after_ supplying a renderer

@docs Att
@docs create
@docs Renderer, AcceptsAttributes
@docs withAttributes
@docs view, viewWith

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)


{-| where `a` is the render target, for example `Html msg`
-}
type alias Att a =
    List (Html.Attribute Never) -> a


{-| -}
type alias Renderer a output =
    AcceptsAttributes a -> output


{-| -}
type alias AcceptsAttributes a =
    { a | additionalAttributes : List (Html.Attribute Never) }


{-| Supply a renderer
-}
create : Renderer a output -> AcceptsAttributes a -> Att output
create howToRender a =
    \additionalAttributes -> howToRender { a | additionalAttributes = a.additionalAttributes ++ additionalAttributes }


{-| -}
withAttributes : List (Html.Attribute Never) -> Att a -> Att a
withAttributes attributes cls =
    \additionalAttributes -> cls (attributes ++ additionalAttributes)


{-| -}
viewWith : List (Html.Attribute Never) -> Att a -> a
viewWith =
    (|>)


{-| -}
view : Att a -> a
view =
    viewWith []
