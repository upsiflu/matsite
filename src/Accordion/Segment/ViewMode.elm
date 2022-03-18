module Accordion.Segment.ViewMode exposing
    ( ViewMode(..)
    , default, periphery, focus, placeholder
    , setPath
    , makeSpine, makeAisle, collapse
    , contextualize
    , isVisible, isExpanded, isAisle, isSpine, path, hide, toClass
    , view
    )

{-| reflects a Segment's position within the Tree

![Accordion Structure](../asset/22-03-17-Accordion.svg)

@docs ViewMode
@docs default, periphery, focus, placeholder


# Map

@docs setPath

@docs makeSpine, makeAisle, collapse

---

@docs contextualize


# Deconstruct

@docs isVisible, isExpanded, isAisle, isSpine, path, hide, toClass


# View

@docs view

-}

import Bool.Extra as Bool
import Css exposing (..)
import Fold exposing (Direction(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)


{-| -}
type ViewMode
    = Focus { expanded : Bool, path : List Direction }
    | Spine { expanded : Bool, path : List Direction }
    | Aisle { expanded : Bool, path : List Direction }
    | Periphery { visible : Bool, path : List Direction }
    | Placeholder


{-| -}
default : ViewMode
default =
    Placeholder


{-| -}
periphery : ViewMode
periphery =
    Periphery { visible = True, path = [] }


{-| -}
focus : ViewMode
focus =
    Focus { expanded = False, path = [] }


{-| -}
placeholder : ViewMode
placeholder =
    Placeholder



---- Map ----


{-| Only peripheric segments can be hidden
-}
hide : ViewMode -> ViewMode
hide mode =
    case mode of
        Periphery config ->
            Periphery { config | visible = False }

        _ ->
            mode


{-| -}
implode : ViewMode -> ViewMode
implode mode =
    Periphery { visible = False, path = path (Debug.log "imploding" mode) }


{-| -}
collapse : ViewMode -> ViewMode
collapse mode =
    case mode of
        Focus config ->
            Focus { config | expanded = False }

        Aisle config ->
            Aisle { config | expanded = False }

        Spine config ->
            Spine { config | expanded = False }

        _ ->
            mode


{-| -}
expand : ViewMode -> ViewMode
expand mode =
    case mode of
        Focus config ->
            Focus { config | expanded = True }

        Aisle config ->
            Aisle { config | expanded = True }

        Spine config ->
            Spine { config | expanded = True }

        _ ->
            mode



----


{-| -}
setPath : List Direction -> ViewMode -> ViewMode
setPath p mode =
    case mode of
        Focus config ->
            Focus { config | path = p }

        Spine config ->
            Spine { config | path = p }

        Aisle config ->
            Aisle { config | path = p }

        Periphery config ->
            Periphery { config | path = p }

        Placeholder ->
            Placeholder


{-| -}
makeAisle : ViewMode -> ViewMode
makeAisle mode =
    case mode of
        Focus config ->
            Focus config

        Spine config ->
            Focus { expanded = False, path = config.path }

        Aisle config ->
            Aisle config

        Periphery config ->
            Aisle { expanded = False, path = config.path }

        Placeholder ->
            Placeholder


{-| -}
makeSpine : ViewMode -> ViewMode
makeSpine mode =
    case mode of
        Focus config ->
            Focus config

        Spine config ->
            Spine config

        Aisle config ->
            Focus config

        Periphery config ->
            Spine { expanded = False, path = config.path }

        Placeholder ->
            Placeholder


{-| Wrapping segments alters their viewMode.

We start only with Focus, Spine and Aisle.

1.  Invisible or Collapsed contexts have invisible offspring
2.  Placeholders bear placeholders
3.  Hide the offspring of peripherals

-}
contextualize : ViewMode -> ViewMode -> ViewMode
contextualize context =
    let
        andIfContext : List (ViewMode -> Bool) -> (ViewMode -> ViewMode) -> (ViewMode -> ViewMode)
        andIfContext tests fu =
            if List.map ((|>) context) tests |> Bool.all then
                fu

            else
                identity

        andIfInnerMode : (ViewMode -> Bool) -> (ViewMode -> ViewMode) -> (ViewMode -> ViewMode)
        andIfInnerMode test fu =
            \innerMode ->
                if test innerMode then
                    fu innerMode

                else
                    innerMode
    in
    identity
        >> andIfContext
            [ isBreadcrumb >> not
            , \ctx -> not (isExpanded ctx) || not (isVisible ctx)
            ]
            (andIfInnerMode
                (isDirectChild >> not)
                hide
            )
        >> andIfContext [ isPlaceholder ]
            (always Placeholder)
        >> andIfContext
            [ isPeriphery ]
            (andIfInnerMode
                (isDirectChild >> not)
                hide
            )
        >> andIfContext [ isFocus ]
            expand
        >> andIfContext [ isAisle, not << isFocus, not << isSpine ]
            implode



---- Deconstruct ----


getProperties :
    ViewMode
    -> { focus : Bool, expanded : Bool, visible : Bool, path : List Direction, spine : Bool, aisle : Bool, periphery : Bool, placeholder : Bool }
getProperties mode =
    let
        empty =
            { focus = False, expanded = False, visible = True, path = [], spine = False, aisle = False, periphery = False, placeholder = False }

        derive props =
            { props
                | focus = props.aisle && props.spine
                , periphery = not props.aisle && not props.spine
            }
    in
    derive <|
        case mode of
            Focus config ->
                { empty
                    | expanded = config.expanded
                    , path = config.path
                    , spine = True
                    , aisle = True
                }

            Spine config ->
                { empty | spine = True, expanded = config.expanded, path = config.path }

            Aisle config ->
                { empty | aisle = True, expanded = config.expanded, path = config.path }

            Periphery config ->
                { empty | visible = config.visible, path = config.path, periphery = True }

            Placeholder ->
                { empty | visible = True, placeholder = True }


{-| -}
path : ViewMode -> List Direction
path =
    getProperties >> .path


{-| -}
isVisible : ViewMode -> Bool
isVisible =
    getProperties >> .visible


{-| -}
isExpanded : ViewMode -> Bool
isExpanded =
    getProperties >> .expanded


{-| -}
isFocus : ViewMode -> Bool
isFocus =
    getProperties >> .focus


{-| -}
isPeriphery : ViewMode -> Bool
isPeriphery =
    getProperties >> .periphery


{-| -}
isPlaceholder : ViewMode -> Bool
isPlaceholder =
    (==) Placeholder


{-| -}
isAisle : ViewMode -> Bool
isAisle =
    getProperties >> .aisle


{-| -}
isSpine : ViewMode -> Bool
isSpine =
    getProperties >> .spine


{-| -}
isBreadcrumb : ViewMode -> Bool
isBreadcrumb =
    path >> List.member Up


{-| -}
isOffspring : ViewMode -> Bool
isOffspring =
    path >> List.member Down


{-| -}
isDirectChild : ViewMode -> Bool
isDirectChild mode =
    case path mode of
        Down :: ath ->
            not (List.member Down ath)

        _ ->
            False



---- View ----


{-| -}
signature : ViewMode -> List ( String, Bool )
signature =
    getProperties
        >> (\p ->
                [ ( "F", p.focus ), ( "S", p.spine ), ( "A", p.aisle ), ( "P", p.periphery ), ( "laceholder", p.placeholder ), ( "e", p.expanded ), ( " (invisible)", not p.visible ) ]
           )


{-| Use for debugging purposes
-}
view : ViewMode -> Html msg
view =
    signature
        >> List.map
            (\( a, b ) ->
                if b then
                    a

                else
                    ""
            )
        >> List.map Html.text
        >> Html.div [ css [ displayFlex ], css [ position absolute, left zero, bottom zero, fontSize (px 12), color (rgb 255 40 0), backgroundColor (rgba 250 0 0 0.1) ] ]


{-| -}
toClass : ViewMode -> Html.Attribute msg
toClass =
    signature >> Attributes.classList
