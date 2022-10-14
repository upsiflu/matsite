module Ui exposing
    ( Ui, Descendant, Item
    , fromHtml, fromTextLabel, fromFoliage
    , Handle(..), toggle, constant
    , with, addLabel, addTextLabel, wrap
    , Flags, toggleFlag, view
    , cacheImg
    , disclose, debugOnly, ifJust, notIf, none
    , Edge(..), overlay
    , sheet
    , pick, pickOrNot, singlePickOrNot, radio
    , check
    , textInput, inputLine
    , toggleButton, toggleModeButton, squareToggleButton
    , distanceHolder, row
    , Face
    , update
    )

{-| Helps separate the state and layout of interface elements from the main model.


## Aspects in the Layout

  - `handle` (Avatars, view-options, hamburger icon...)
  - `scene` (the objects of interest)
  - `control` (tools and config sheet)
  - `info` (snacks and status)

Normally, composed `Ui`s will output a flat list of DOM elements for each region.
You can `wrap` a nested aspect to create deeper nestings in the DOM.


## Contingent rendering

`Flags` correspond to `Handle`s.
Turning off a flag renders invisible the corresponding `control` with its descendants, as well as
one-layer deep nested controls with their descendants.

---

@docs Ui, Descendant, Item


# Create


### Html

@docs fromHtml, fromTextLabel, fromFoliage


### Handle

@docs Handle, toggle, constant


# Map

There is no `map`... need to learn how to `Html.map` a function.


# Compose

_Note that `Ui`s are `Lists`, so you can use `++`, `List.reverse` etc!_

@docs with, addLabel, addTextLabel, wrap


# View

@docs Flags, toggleFlag, view

---


# HTML helpers

@docs cacheImg
@docs disclose, debugOnly, ifJust, notIf, none


## Overlays

@docs Edge, overlay


## Controls

@docs sheet
@docs pick, pickOrNot, singlePickOrNot, radio
@docs check
@docs textInput, inputLine

---

@docs toggleButton, toggleModeButton, squareToggleButton


## Layout

@docs distanceHolder, row

@docs Face

-}

import Bool.Extra as Bool exposing (ifElse)
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed exposing (node, ul)
import Json.Decode exposing (maybe)
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Get as Get exposing (Get)
import Ui.Layout as Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel as ViewModel exposing (Foliage, ViewModel)
import Ui.Mask as Mask exposing (Mask)
import Url exposing (Url)
import Zipper exposing (Zipper)


{-| Items in a Ui are mutually independent.
A Ui itself has no aspect.
-}
type alias Ui msg =
    List (Descendant msg)


{-| -}
type Descendant msg
    = Leaf (Foliage msg) (Maybe (Item msg))
    | Wrap (Foliage msg -> Foliage msg) (Ui msg)


{-| -}
type alias Item msg =
    { handle : Maybe (Handle msg)
    , get : Get (Ui msg)
    }



---- CREATE ----


{-| -}
fromTextLabel : String -> Ui msg
fromTextLabel =
    addTextLabel >> (|>) []


{-| -}
fromHtml : Html msg -> Ui msg
fromHtml =
    Tuple.pair "" >> List.singleton >> fromFoliage


{-| -}
fromFoliage : Foliage msg -> Ui msg
fromFoliage =
    Leaf >> (|>) Nothing >> List.singleton


{-| -}
type Handle msg
    = Constant (List (Html msg))
    | Toggle String (Html Never)


createHandle : Handle msg -> Ui msg
createHandle h =
    fromItem { handle = Just h, get = \_ -> Nothing }


{-| Adds a binary flag; is represented by a togglebutton.
-}
toggle : String -> Html Never -> Ui msg
toggle =
    Toggle >> (<<) createHandle


{-| -}
constant : List (Html msg) -> Ui msg
constant =
    Constant >> createHandle



---- COMPOSE ----


{-| Each descendant in the ui gets the same additional sub-Ui, via an aspect.
-}
with : Aspect -> Ui msg -> Ui msg -> Ui msg
with aspect additional =
    List.map
        (\original ->
            case original of
                Leaf foliage maybeItem ->
                    Maybe.unpack
                        (\() -> { handle = Nothing, get = Get.singleton aspect additional })
                        (\it -> { it | get = Get.addList aspect additional it.get })
                        maybeItem
                        |> Just
                        |> Leaf foliage

                Wrap fu ui ->
                    Wrap fu (with aspect additional ui)
        )


{-| Nest the DOM here.
If you wrap, and then define the contextual aspect,
the wrapper will wrap all descendants that constitute this aspect.

    example : Ui msg
    example =
        []
            |> with Scene []
            |> wrap ((++) ( "message", Html.text "I am wrapped" ))
            |> with Control []

Now, let's see what happens if we define a contextual aspect.

    []
        |> with Control example

This will output:
`Scene -> []`,
`Control -> "I am wrapped" []`

-}
wrap : (Foliage msg -> Foliage msg) -> Ui msg -> Ui msg
wrap =
    Wrap >> (<<) List.singleton


fromItem : Item msg -> Ui msg
fromItem =
    Just >> Leaf [] >> List.singleton


{-| prepends a freeform label to the contextual aspect
-}
addLabel : Ui msg -> Ui msg -> Ui msg
addLabel l =
    (++) l >> wrap (node "label" [] >> Tuple.pair "" >> List.singleton)


{-| prepends a text label to the contextual aspect
-}
addTextLabel : String -> Ui msg -> Ui msg
addTextLabel =
    Html.text
        >> List.singleton
        >> Html.span [ class "text label" ]
        >> fromHtml
        >> addLabel



---- VIEW ----


sameLayerMask : Mask (Ui msg)
sameLayerMask =
    Mask.occludeList
        [ Control, Info ]


lowerLayerMask : Mask (Ui msg)
lowerLayerMask =
    Mask.occludeList
        [ Control, Info ]



----


{-| The state of the whole Ui can be mapped directly to the _Url query_ as a list of flags
-}
type alias Flags =
    List String


{-| -}
toggleFlag : String -> Flags -> Flags
toggleFlag flag flags =
    flags
        |> (if List.member flag flags then
                List.remove flag

            else
                (::) flag
           )


{-| -}
update : Url -> Url
update =
    identity


{-| -}
view : Flags -> Maybe Layout -> Ui msg -> Foliage msg
view flags maybeLayout =
    let
        withFlag : String -> String
        withFlag q =
            "?" ++ String.join "&" (q :: List.remove "" flags)

        withoutFlag : String -> String
        withoutFlag q =
            "?" ++ String.join "&" (List.remove q flags)

        viewHandle : Maybe (Handle msg) -> ( Foliage msg, Bool )
        viewHandle =
            Maybe.map
                (\h ->
                    case h of
                        Constant html ->
                            ( keyByIndex html, True )

                        Toggle flag face ->
                            if List.member flag flags then
                                ( keyByIndex [ Html.a [ href ("?toggle=" ++ flag), attribute "role" "switch", attribute "aria-checked" "true" ] [ Html.map never face ] ], True )

                            else
                                ( keyByIndex [ Html.a [ href ("?toggle=" ++ flag), attribute "role" "switch", attribute "aria-checked" "false" ] [ Html.map never face ] ], False )
                )
                >> Maybe.withDefault ( [], True )

        viewUi : (Descendant msg -> ViewModel msg) -> Ui msg -> ViewModel msg
        viewUi how =
            List.foldl (how >> ViewModel.merge) ViewModel.empty

        viewGet : ( Aspect, Mask (Ui msg) ) -> Get (Ui msg) -> ViewModel msg
        viewGet ( aspect, mask ) =
            Get.mapByKey (\key -> viewUi (viewDesc ( key, mask )))
                >> Get.values [ Scene, {- Control, -} Info ]
                >> List.foldl ViewModel.merge ViewModel.empty

        viewItem : ( Aspect, Mask (Ui msg) ) -> Item msg -> ViewModel msg
        viewItem ( aspect, mask ) item =
            let
                ( handle, isOn ) =
                    viewHandle item.handle
            in
            viewGet ( aspect, mask ) item.get
                |> ViewModel.mapHandle (\_ -> handle)

        viewDesc : ( Aspect, Mask (Ui msg) ) -> Descendant msg -> ViewModel msg
        viewDesc ( aspect, mask ) d =
            case d of
                Leaf foliage maybeItem ->
                    maybeItem
                        |> Maybe.unwrap ViewModel.empty (viewItem ( aspect, mask ))
                        |> ViewModel.mapGet (Get.addList aspect foliage)

                Wrap wrapper descList ->
                    descList
                        |> viewUi (viewDesc ( aspect, mask ))
                        |> ViewModel.mapGet (Get.update aspect wrapper)
    in
    viewUi (viewDesc ( Scene, Mask.transparent ))
        >> Layout.view (Maybe.withDefault Layout.Default maybeLayout)



---- Conditional Views ----


isDebugging : Bool
isDebugging =
    False


{-| -}
debugOnly : Html msg -> Html msg
debugOnly =
    notIf (not isDebugging)


{-| -}
ifJust : (a -> Html msg) -> Maybe a -> Html msg
ifJust fu =
    Maybe.map fu >> Maybe.withDefault (Html.text "")


{-| -}
notIf : Bool -> Html msg -> Html msg
notIf =
    ifElse
        (\_ -> Html.text "")
        identity


{-| -}
none : Html msg
none =
    Html.text ""


keyByIndex : List (Html msg) -> Foliage msg
keyByIndex =
    List.indexedMap (String.fromInt >> Tuple.pair)



---- Overlays


{-| -}
type Edge
    = Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | TopLeft


{-| -}
overlay : Edge -> List (Html msg) -> Html msg
overlay edge =
    let
        attr =
            case edge of
                Top ->
                    [ top zero, left (pct 50) ]

                TopRight ->
                    [ top zero, right zero ]

                Right ->
                    [ top (pct 50), right zero ]

                BottomRight ->
                    [ bottom zero, right zero ]

                Bottom ->
                    [ bottom zero, left (pct 50) ]

                BottomLeft ->
                    [ bottom zero, left zero ]

                Left ->
                    [ top (pct 50), left zero ]

                TopLeft ->
                    [ top zero, left zero ]
    in
    Html.div
        [ class "overlay", [ position absolute, color (rgb 255 255 0), backgroundColor (rgba 255 255 0 0.1), Css.property "writing-mode" "horizontal-tb" ] ++ attr |> css ]



---- Controls


{-| -}
row : List (Html msg) -> Html msg
row =
    Html.fieldset [ class "row" ]


{-| -}
sheet : List (Html msg) -> Html msg
sheet contents =
    Html.details [ class "sheet", attribute "open" "True" ] <| contents ++ [ Html.summary [ class "collapseSheet" ] [ Html.span [] [ Html.text "Properties" ] ] ]



---- ViewModel


{-| Examples:

    OneOf "Yes or no?" (Zipper.create ( Html.text "Yes", Answer True ) [] [ ( Html.text "No", Answer False ) ])

    ZeroOrOneOf "Perhaps..." False (Answer Nothing) (Zipper.singleton ( Html.text "Yes", Answer (Just "Yes") ))

-}
type Field msg
    = OneOf String (Zipper ( Html msg, msg ))
    | ZeroOrOneOf String Bool msg (Zipper ( Html msg, msg ))
    | StringInput String { data : String, save : String -> msg }


{-| -}
pick : Zipper ( Face, Maybe msg ) -> Html msg
pick =
    pickHelp "pick" True


{-| -}
pickOrNot : Bool -> Zipper ( Face, Maybe msg ) -> Html msg
pickOrNot =
    pickHelp "pickOrNot"


pickHelp : String -> Bool -> Zipper ( Face, Maybe msg ) -> Html msg
pickHelp className isActive =
    Zipper.map (Tuple.pair False)
        >> (if isActive then
                Zipper.mapFocus (Tuple.mapFirst not)

            else
                identity
           )
        >> Zipper.flat
        >> List.map
            (\( isChecked, ( face, t ) ) -> radio face t isChecked)
        >> div [ class "ui", class className ]


{-| -}
inputLine : String -> String -> String -> (String -> msg) -> Html msg
inputLine cls ttl val msg =
    label [ class "ui" ] [ Html.input [ class cls, title ttl, type_ "input", value val, onInput msg ] [] ]


{-| -}
singlePickOrNot : Bool -> ( Face, Maybe msg ) -> Html msg
singlePickOrNot isActive =
    Zipper.singleton >> pickOrNot isActive


{-| -}
radio : Face -> Maybe msg -> Bool -> Html msg
radio face t isChecked =
    label [ class "ui", title face.title ]
        [ input (quadState isChecked t ++ [ type_ "radio", Attributes.checked isChecked ]) []
        , span [] face.front |> Html.map never
        ]


{-|

    myFace =
        [ Html.Styled.text ":-)" ]
            |> Face "This is my face"

-}
type alias Face =
    { title : String, front : List (Html Never) }


{-| -}
check : Face -> msg -> Maybe Bool -> Html msg
check face t isChecked =
    Html.label
        [ class "ui", title face.title, triState isChecked, onClick t ]
        [ Html.input [ type_ "checkbox", triState isChecked ] [], Html.span [] face.front |> Html.map never ]


{-| -}
textInput : String -> String -> Maybe (String -> msg) -> Html msg
textInput hint val send =
    label [ class "ui" ] [ Html.input [ title hint, type_ "text", send |> Maybe.map onInput |> Maybe.withDefault (Attributes.disabled True), value val ] [] ]


triState : Maybe Bool -> Attribute msg
triState isChecked =
    case isChecked of
        Nothing ->
            Attributes.disabled True

        Just True ->
            Attributes.checked True

        Just False ->
            Attributes.checked False


quadState : Bool -> Maybe msg -> List (Attribute msg)
quadState isChecked activate =
    Maybe.map (\msg -> [ Attributes.disabled False, onClick msg ]) activate
        |> Maybe.withDefault [ Attributes.disabled True ]
        |> (++)
            [ Attributes.attribute "aria-checked"
                (if isChecked then
                    "true"

                 else
                    "false"
                )
            ]


{-| -}
toggleModeButton : Face -> Bool -> Maybe msg -> Html msg
toggleModeButton face isChecked t =
    let
        attr =
            t
                |> Maybe.map
                    (onClick >> List.singleton)
                |> Maybe.withDefault []
                |> (++) (quadState isChecked t)
                |> (++) [ title face.title, class "ui mode stretching" ]
    in
    List.map (Html.map never) face.front
        |> Html.button attr


{-| -}
toggleButton : Face -> Bool -> Maybe msg -> Html msg
toggleButton face isChecked t =
    let
        attr =
            t
                |> Maybe.map
                    (onClick >> List.singleton)
                |> Maybe.withDefault []
                |> (++) (quadState isChecked t)
                |> (++) [ title face.title, class "ui stretching" ]
    in
    List.map (Html.map never) face.front
        |> Html.button attr


{-| -}
squareToggleButton : Face -> Bool -> Maybe msg -> Html msg
squareToggleButton face isChecked t =
    List.map (Html.map never) face.front
        |> Html.button (quadState isChecked t ++ [ title face.title, class "ui square" ])


{-| -}
distanceHolder : Html msg
distanceHolder =
    Html.div [ class "distance-holder" ] []


{-| Caches an optimally downsized image at weserv.nl

  - description: `title` attribute
  - diameter: the desired number of columns (each 21 rem wide) (height will be clamped to 768px)
  - cls: CSS class (space-separated string)
  - location: Url of the original file

-}
cacheImg : String -> Int -> String -> String -> Html msg
cacheImg description diameter cls location =
    let
        columnWidth =
            21 * 16

        ( w, h ) =
            ( String.fromInt (Basics.min 2 diameter * columnWidth), "768" )
    in
    Html.img
        [ title description
        , class cls
        , src <| "https://images.weserv.nl/?url=" ++ location ++ "&w=" ++ w ++ "&h=" ++ h ++ "&fit=inside&we&filename=" ++ description
        ]
        []



---- Patterns ----


{-| given expandable content and a header, generate a `<details><summary>` dropdown
-}
disclose : List (Html msg) -> List (Html msg) -> Html msg
disclose more handle =
    details [] [ summary [] handle, div [ class "popup" ] more ]
