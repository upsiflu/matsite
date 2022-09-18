module Ui exposing
    ( Ui, Item
    , map
    , append, concat
    , cacheImg
    , disclose, debugOnly, ifJust, notIf, none
    , view
    , Edge(..), overlay
    , sheet
    , pick, pickOrNot, singlePickOrNot, radio
    , check
    , textInput, inputLine
    , toggleButton, toggleModeButton, squareToggleButton
    , distanceHolder
    , Face
    , Handle(..), State, fromHandle, fromHtml, fromHtmlList, row, singleton, withControl, withScene, wrapScene
    )

{-| Gui Helpers

@docs Ui, Item


# Create

@docs empty


# Map

@docs map, mapList


# Compose

@docs append, concat
@docs composeScenes, composeControls


# HTML helpers

@docs cacheImg
@docs disclose, debugOnly, ifJust, notIf, none


# View

@docs view


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

@docs distanceHolder

@docs Face

-}

import Bool.Extra as Bool exposing (ifElse)
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed exposing (ul)
import List.Extra as List
import Maybe.Extra as Maybe
import Zipper exposing (Zipper)


{-| consists of several `item`s that may be orthogonal to each other.
`Sub` Uis have two nestings:

  - Logical nesting; per item (see `Item`)
  - DOM nesting; per Ui:
    All scenes inside a `Sub` are concatenated, and as such transformed according to
    the second parameter of `Sub`.

-}
type Ui msg
    = Sub (List (Item msg)) (List ( String, Html msg ) -> List ( String, Html msg ))
    | Leaf (List (Html msg))


fromHtml : Html msg -> Ui msg
fromHtml =
    List.singleton >> Leaf


{-| will render as `scene` with integer keys
-}
fromHtmlList : List (Html msg) -> Ui msg
fromHtmlList =
    Leaf


{-| The state of the whole Ui can be mapped directly to the query as a list of flags
-}
type alias State =
    List String


{-| State applies to handles: an 'open' handle corresponds to its name appearing in the State
-}
type Handle msg
    = Constant (Html msg)
    | Toggle String (Html Never)


mapHandle : (a -> b) -> Handle a -> Handle b
mapHandle fu h =
    case h of
        Constant html ->
            Constant (Html.map fu html)

        Toggle flag face ->
            Toggle flag face


{-|

  - control: global toolbar or property sheet
  - handle: the item's avatar or menu, containing a unique flag
  - info: statusbar, help screen, or tooltip bubbles
  - scene: the item's editable contents and overlays, each with a unique key

The State pertaining to the handle determines whether the control is drawn or not.

-}
type alias Item msg =
    { handle : Maybe (Handle msg)
    , scene : List ( String, Ui msg )
    , info : Maybe (Ui msg)
    , control : Maybe (() -> Ui msg)
    }


singleton : Ui msg
singleton =
    Sub [ void ] identity


{-| sets the handle in a new Ui.
You cannot change the handle later, or add handles to an existing Ui.
What you can do is, create multiple Uis with one handle each, and then combine
them with append and concat. This way you can build multi-faceted Uis.
If the Ui is not segmented, it is wrapped into an initial `scene`.
-}
fromHandle : Handle msg -> Ui msg
fromHandle h =
    mapFirst (\i -> { i | handle = Just h }) singleton



{- Helper to promote and populate a Ui. If the Ui is not segmented, it is wrapped into an initial `scene`. -}


mapFirst : (Item msg -> Item msg) -> Ui msg -> Ui msg
mapFirst fu ui =
    case ui of
        Leaf l ->
            mapFirst fu (promote ui)

        Sub (i :: tems) transform ->
            Sub (fu i :: tems) transform

        Sub [] transform ->
            Sub [] transform


{-| Adds a scene in the first item of the Ui, before any existing scenes.
Scenes are rendered as list items, with attributes applied to the `Keyed.li`.
Make sure that key strings are unique, preferably globally!
-}
withScene : List ( String, Ui msg ) -> Ui msg -> Ui msg
withScene s =
    mapFirst
        (\item ->
            { item
                | scene = s ++ item.scene
            }
        )


{-| appends a control to the first item in the Ui
-}
withControl : (() -> Ui msg) -> Ui msg -> Ui msg
withControl c =
    mapFirst
        (\item ->
            { item
                | control =
                    case item.control of
                        Just fu ->
                            Just (\() -> fu () |> append (c ()))

                        Nothing ->
                            Just c
            }
        )


{-| appends into to the first item in the Ui
-}
withInfo : Ui msg -> Ui msg -> Ui msg
withInfo i =
    mapFirst
        (\item ->
            { item
                | info =
                    case item.info of
                        Just ui ->
                            Just (append i ui)

                        Nothing ->
                            Just i
            }
        )


void : Item msg
void =
    Item Nothing [] Nothing Nothing



---- MODIFY ----


{-| modify the message type
Attention: The wrapper is removed
-}
map : (a -> b) -> Ui a -> Ui b
map fu ui =
    let
        mapItem i =
            { handle = Maybe.map (mapHandle fu) i.handle
            , scene = List.map (Tuple.mapSecond (map fu)) i.scene
            , info = Maybe.map (map fu) i.info
            , control = Maybe.map (\c -> c >> map fu) i.control
            }
    in
    case ui of
        Sub ll transform ->
            Sub (List.map mapItem ll) identity

        Leaf ll ->
            Leaf (List.map (Html.map fu) ll)


promote : Ui msg -> Ui msg
promote ui =
    case ui of
        Sub _ _ ->
            ui

        leaf ->
            singleton |> withScene [ ( "!", leaf ) ]



---- COMPOSE ----


{-| compose two `Gui`s into one, making all `Item`s orthogonal to each other
-}
append : Ui msg -> Ui msg -> Ui msg
append a b =
    case ( a, b ) of
        ( Sub aa ta, Sub bb tb ) ->
            Sub (aa ++ bb) (ta >> tb)

        ( Leaf aa, Leaf bb ) ->
            Leaf (aa ++ bb)

        ( sub, Leaf bb ) ->
            append sub (promote (Leaf bb))

        ( Leaf aa, sub ) ->
            append (promote (Leaf aa)) sub


{-| compose many `Gui`s into one
Fails to Nothing on []
-}
concat : List (Ui item) -> Maybe (Ui item)
concat =
    List.foldl1 append


wrapScene : (List ( String, Html msg ) -> List ( String, Html msg )) -> Ui msg -> Ui msg
wrapScene fu ui =
    case ui of
        Sub items transform ->
            Sub items (transform >> fu)

        _ ->
            wrapScene fu (promote ui)



---- VIEW ----


{-| given expandable content and a header, generate a `<details><summary>` dropdown
-}
disclose : List (Html msg) -> List (Html msg) -> Html msg
disclose more handle =
    details [] [ summary [] handle, div [ class "popup" ] more ]


{-| Magic happens here.
For example, we have two `view`able types, `Segment` and `Accordion`.

`Segment.view` renders a `Ui` singleton where

  - the `scene` contains a list of `(String, Html msg)` elements
  - the `control` contains editing controls and also the overlays `scene`.
  - we `concat` all Segments (they are orthogonal)

`Accordion.view` renders a `Ui` singleton where

  - the `handle` has a toggle for the `control`
  - the `control` can edit the whole Accordion
  - the `scene` has overlaid frames for regions (screenBackground, aisleBackground, xy, closest-aisle)
  - we insert the `Segments` as a `scene` (hierarchically controlled by the handle)
  - we `append` a singleton `handle` with unit state for the hamburger menu

Now, flipping the Accordion item handle toggles the rendering of its `control`:
When it is off, the control is not rendered, and the Segment scene inside it
(the overlays) is also not rendered.
When it is on, the control is rendered and the overlays are added to the scene.

Questions:

1.  How do we wrap the `overlay` over the segment-view?
    Answer: we don't. Overlays and underlays inhabit the same pool and are overlaid by CSS, not by DOM nesting.
2.  How do we nest controls into a fieldsets?
    Answer: Perhaps that's not necessary. We create the fieldsets for parent and child
    plus the row (not fieldset) for Accordion, and then they are wrapped globally into a Sheet,
    which may retain the contract/expand-from-below capability through a global 'control' property.

-}
view : State -> Ui msg -> List ( String, Html msg )
view state ui =
    let
        autoIndex =
            List.indexedMap (\index item -> ( String.fromInt index, item ))

        viewHandle : Handle msg -> Html msg
        viewHandle h =
            case h of
                Constant html ->
                    html

                Toggle flag face ->
                    Html.map never face

        -- Delete control out of a scene:
        viewSubScene : Bool -> ( String, Ui msg ) -> List ( String, Html msg )
        viewSubScene isControlVisible ( key, scene ) =
            case scene of
                Leaf htmls ->
                    [ ( key, ul [] (autoIndex htmls) ) ]

                Sub items transform ->
                    view state
                        (Sub
                            (items
                                |> List.map
                                    (\item ->
                                        { item
                                            | control =
                                                if isControlVisible then
                                                    item.control

                                                else
                                                    Nothing
                                            , scene = List.map (\( subKey, subScene ) -> ( key ++ "-" ++ subKey, subScene )) item.scene
                                        }
                                    )
                            )
                            transform
                        )
    in
    case ui of
        Leaf htmls ->
            autoIndex htmls

        Sub items transform ->
            items
                |> List.foldl
                    (\item acc ->
                        let
                            isOn =
                                case item.handle of
                                    Just (Constant _) ->
                                        True

                                    Just (Toggle flag _) ->
                                        List.member flag state

                                    Nothing ->
                                        False
                        in
                        { acc
                            | handles = Maybe.cons item.handle acc.handles
                            , scenes = List.concatMap (viewSubScene isOn) item.scene ++ acc.scenes
                            , infos = Maybe.cons item.info acc.infos
                            , controls =
                                case ( item.control, isOn ) of
                                    ( Just control, True ) ->
                                        control () :: acc.controls

                                    _ ->
                                        acc.controls
                        }
                    )
                    { handles = []
                    , scenes = []
                    , infos = []
                    , controls = []
                    }
                |> (\i ->
                        autoIndex (List.map viewHandle i.handles)
                            -- scene: only show control within scene if parent handle is on
                            ++ transform i.scenes
                            ++ autoIndex (List.map (view state >> ul []) i.infos)
                            ++ autoIndex (List.map (view state >> ul []) i.controls)
                   )



---- Conditional Views


isDebugging : Bool
isDebugging =
    False


{-| -}
debugOnly : Html msg -> Html msg
debugOnly =
    notIf (not isDebugging)


{-| -}
ifJust : (a -> Html msg) -> Maybe a -> Html msg
ifJust transform =
    Maybe.map transform >> Maybe.withDefault (Html.text "")


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
    Html.div [ class "row" ]


{-| -}
sheet : List (Html msg) -> Html msg
sheet contents =
    Html.details [ class "sheet", attribute "open" "True" ] <| contents ++ [ Html.summary [ class "collapseSheet" ] [ Html.span [] [ Html.text "Properties" ] ] ]



---- ViewModel


type alias ViewModel msg =
    List (Field msg)


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
            (\( isChecked, ( face, toggle ) ) -> radio face toggle isChecked)
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
radio face toggle isChecked =
    label [ class "ui", title face.title ]
        [ input (quadState isChecked toggle ++ [ type_ "radio", Attributes.checked isChecked ]) []
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
check face toggle isChecked =
    Html.label
        [ class "ui", title face.title, triState isChecked, onClick toggle ]
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
toggleModeButton face isChecked toggle =
    let
        attr =
            toggle
                |> Maybe.map
                    (onClick >> List.singleton)
                |> Maybe.withDefault []
                |> (++) (quadState isChecked toggle)
                |> (++) [ title face.title, class "ui mode stretching" ]
    in
    List.map (Html.map never) face.front
        |> Html.button attr


{-| -}
toggleButton : Face -> Bool -> Maybe msg -> Html msg
toggleButton face isChecked toggle =
    let
        attr =
            toggle
                |> Maybe.map
                    (onClick >> List.singleton)
                |> Maybe.withDefault []
                |> (++) (quadState isChecked toggle)
                |> (++) [ title face.title, class "ui stretching" ]
    in
    List.map (Html.map never) face.front
        |> Html.button attr


{-| -}
squareToggleButton : Face -> Bool -> Maybe msg -> Html msg
squareToggleButton face isChecked toggle =
    List.map (Html.map never) face.front
        |> Html.button (quadState isChecked toggle ++ [ title face.title, class "ui square" ])


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
