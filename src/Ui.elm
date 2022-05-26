module Ui exposing (..)

{-| Gui Helpers
-}

import Bool.Extra as Bool exposing (ifElse)
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Occurrence exposing (Occurrence)
import Zipper exposing (Zipper)


{-| consists of several `Item`s that are somewhat orthogonal to each other.
-}
type Ui msg
    = Ui (List (Item msg))


{-| -}
type alias Item msg =
    { handle : Html msg, scene : ( String, Html msg ), info : Html msg, control : Html msg }


{-| Create a Gui out of a single Item. Modify the empty Item.
-}
fromEmpty : (Item msg -> Item msg) -> Ui msg
fromEmpty fu =
    Item none ( "", none ) none none
        |> fu
        |> singleton


{-| `info` is a list of dismissible or disclosable messages (for example toasts),
`handle` comprises the permanent handles to the object (for example, an avatar plus a login/logout button)
and that potentially includes custom-elements for syncing with a backend.
`scene` represents data that can be manipulated in-place.
`control` is the set of tools related to the object.
-}
singleton : Item msg -> Ui msg
singleton =
    List.singleton >> Ui



---- MODIFY ----


{-| -}
map : (a -> b) -> Ui a -> Ui b
map fu (Ui items) =
    let
        mapFacet =
            Html.map fu

        mapItem i =
            { handle = mapFacet i.handle
            , scene = Tuple.mapSecond mapFacet i.scene
            , info = mapFacet i.info
            , control = mapFacet i.control
            }
    in
    Ui (List.map mapItem items)



---- COMPOSE ----


{-| compose two `Gui`s into one
-}
with : Ui msg -> Ui msg -> Ui msg
with (Ui l0) (Ui l1) =
    Ui (l0 ++ l1)


{-| compose many `Gui`s into one
-}
concat : List (Ui msg) -> Ui msg
concat =
    List.foldl with (fromEmpty identity)


{-| encloses all scenes within the Ui into a single scene
-}
composeScenes : (List ( String, Html msg ) -> ( String, Html msg )) -> Ui msg -> Ui msg
composeScenes fu (Ui l) =
    Item none (fu (List.map .scene l)) none none
        :: List.map (\item -> { item | scene = ( "", none ) }) l
        |> Ui


{-| encloses all controls within the Ui into a single scene
-}
composeControls : (List (Html msg) -> Html msg) -> Ui msg -> Ui msg
composeControls fu (Ui l) =
    Item none ( "", none ) none (fu (List.map .control l))
        :: List.map (\item -> { item | control = none }) l
        |> Ui



---- VIEW ----


{-| -}
disclose : List (Html msg) -> List (Html msg) -> Html msg
disclose more handle =
    details [] [ summary [] handle, div [ class "popup" ] more ]


{-| -}
view : Ui msg -> List ( String, Html msg )
view (Ui items) =
    let
        indexicate =
            List.indexedMap (\index item -> ( String.fromInt index, item ))
    in
    items
        |> List.foldl
            (\item acc ->
                { acc
                    | handles = item.handle :: acc.handles
                    , scenes = item.scene :: acc.scenes
                    , infos = item.info :: acc.infos
                    , controls = item.control :: acc.controls
                }
            )
            { handles = []
            , scenes = []
            , infos = []
            , controls = []
            }
        |> (\i -> i.scenes ++ indexicate i.handles ++ indexicate i.infos ++ indexicate i.controls)



---- Conditional Views


isDebugging : Bool
isDebugging =
    False


debugOnly : Html msg -> Html msg
debugOnly =
    notIf (not isDebugging)


ifJust : (a -> Html msg) -> Maybe a -> Html msg
ifJust transform =
    Maybe.map transform >> Maybe.withDefault (Html.text "")


notIf : Bool -> Html msg -> Html msg
notIf =
    ifElse
        (\_ -> Html.text "")
        identity


none : Html msg
none =
    Html.text ""



---- Decorations


type Edge
    = Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | TopLeft


sheet : List (Html msg) -> Html msg
sheet contents =
    Html.details [ class "sheet", attribute "open" "True" ] <| contents ++ [ Html.summary [ class "collapseSheet" ] [ Html.span [] [ Html.text "Properties" ] ] ]


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
    | ManyOccurrences
        String
        { data : List Occurrence
        , edit : Occurrence -> Occurrence -> msg
        , add : Occurrence -> msg
        , remove : Occurrence -> msg
        }
    | StringInput String { data : String, save : String -> msg }


pick : Zipper ( Face, Maybe msg ) -> Html msg
pick =
    pickHelp "pick" True


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


singlePickOrNot : Bool -> ( Face, Maybe msg ) -> Html msg
singlePickOrNot isActive =
    Zipper.singleton >> pickOrNot isActive


radio : Face -> Maybe msg -> Bool -> Html msg
radio face toggle isChecked =
    label [ class "ui" ]
        [ input (quadState isChecked toggle ++ [ type_ "radio", Attributes.checked isChecked ]) []
        , span [] face.front |> Html.map never
        ]


type alias Face =
    { front : List (Html Never), title : String }


check : Face -> msg -> Maybe Bool -> Html msg
check face toggle isChecked =
    Html.label
        [ class "ui", title face.title, triState isChecked, onClick toggle ]
        [ Html.input [ type_ "checkbox", triState isChecked ] [], Html.span [] face.front |> Html.map never ]


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


squareToggleButton : Face -> Bool -> Maybe msg -> Html msg
squareToggleButton face isChecked toggle =
    List.map (Html.map never) face.front
        |> Html.button (quadState isChecked toggle ++ [ title face.title, class "ui square" ])


distanceHolder : Html msg
distanceHolder =
    Html.div [ class "distance-holder" ] []


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
