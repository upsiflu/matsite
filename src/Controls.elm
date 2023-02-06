module Controls exposing
    ( cacheImg, disclose
    , Edge(..), overlay
    , sheet
    , pick, pickOrNot, singlePickOrNot, radio
    , check
    , textInput, inputLine
    , toggleButton, toggleModeButton, squareToggleButton
    , distanceHolder, row
    , Face
    )

{-|


# Working with Foliage (keyed `Html`)

@docs cacheImg, disclose


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
import Browser exposing (Document)
import Browser.Navigation as Nav
import Css exposing (..)
import Html as Unstyled
import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed exposing (node)
import List.Extra as List
import Maybe.Extra as Maybe
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout as Layout exposing (Layout)
import Restrictive.Layout.Region exposing (Aspect(..))
import Restrictive.Mask as Mask exposing (Mask)
import Restrictive.State exposing (State)
import Tuple exposing (pair)
import Url exposing (Url)
import Url.Codec exposing (Codec)
import Zipper exposing (Zipper)



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
