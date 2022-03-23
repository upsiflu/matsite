module Accordion.Segment.ViewMode exposing
    ( ViewMode(..), Role(..)
    , role, path
    , view, toClass
    )

{-| reflects a Segment's position within the Tree

![Accordion Structure](../asset/22-03-17-Accordion.svg)

@docs ViewMode, Role


# Deconstruct

@docs role, path


# View

@docs view, toClass

-}

import Bool.Extra as Bool
import Css exposing (..)
import Fold exposing (Direction(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import List.Extra as List


{-| -}
type ViewMode
    = Default Position
    | Collapsed Position
    | Placeholder


type alias Position =
    { path : List Direction, isLeaf : Bool, isRoot : Bool }


{-| The Role is a human-readable representation of the path
-}
type Role
    = Parent
    | Focus
    | Aisle
    | Breadcrumb
    | BreadcrumbAisle
    | Periphery
    | None


{-| -}
role : ViewMode -> Role
role mode =
    case path mode of
        Nothing ->
            None

        Just [] ->
            Focus

        Just [ Up ] ->
            Parent

        Just (Up :: s) ->
            if List.member Down s then
                Periphery

            else if List.all ((==) Up) s then
                Breadcrumb

            else
                BreadcrumbAisle

        Just s ->
            if List.member Down s then
                Periphery

            else
                Aisle


{-| -}
path : ViewMode -> Maybe (List Direction)
path mode =
    case mode of
        Default p ->
            Just p.path

        _ ->
            Nothing



---- View ----


{-| -}
signature : ViewMode -> String
signature mode =
    case role mode of
        Parent ->
            "P"

        Focus ->
            "F"

        Aisle ->
            "A"

        Breadcrumb ->
            "B"

        BreadcrumbAisle ->
            "Ba"

        Periphery ->
            "p"

        None ->
            "[-]"


{-| Use for debugging purposes
-}
view : ViewMode -> Html msg
view =
    signature
        >> Html.text
        >> List.singleton
        >> Html.div [ css [ displayFlex ], css [ position absolute, left zero, bottom zero, fontSize (px 12), color (rgb 255 40 0), backgroundColor (rgba 250 0 0 0.1) ] ]


{-| -}
toClass : ViewMode -> Html.Attribute msg
toClass =
    signature >> Attributes.class
