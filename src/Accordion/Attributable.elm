module Accordion.Attributable exposing
    ( create
    , view, viewWith
    , Att, withAttributes
    )

{-| A builder that allows for adding classes later

@docs Cls
@docs create
@docs withClasses
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


{-| Supply a renderer
-}
create : ({ a | additionalAttributes : List (Html.Attribute Never) } -> output) -> { a | additionalAttributes : List (Html.Attribute Never) } -> Att output
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
