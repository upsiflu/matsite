module Ui exposing (..)

{-| Gui Helpers
-}

import Bool.Extra exposing (ifElse)
import Css exposing (..)
import Html.Styled as Html exposing (Html, fieldset, input, label, span, text)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Occurrence exposing (Occurrence)
import Zipper exposing (Zipper)
import Zipper.Mixed as MixedZipper exposing (MixedZipper)



---- Conditional Views


isDebugging =
    False


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
    Html.details [ class "sheet", attribute "open" "True" ] <| contents ++ [ Html.summary [] [ Html.span [] [ Html.text "Properties" ] ] ]


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


type alias Face =
    Html Never


{-| Examples:

    OneOf "Yes or no?" (Zipper.create ( Html.text "Yes", Answer True ) [] [ ( Html.text "No", Answer False ) ])

    ZeroOrOneOf "Perhaps..." False (Answer Nothing) (Zipper.singleton ( Html.text "Yes", Answer (Just "Yes") ))

-}
type Field msg
    = OneOf String (Zipper ( Face, msg ))
    | ZeroOrOneOf String Bool msg (Zipper ( Face, msg ))
    | ManyOccurrences
        String
        { data : List Occurrence
        , edit : Occurrence -> Occurrence -> msg
        , add : Occurrence -> msg
        , remove : Occurrence -> msg
        }
    | StringInput String { data : String, save : String -> msg }


pick : Zipper ( String, msg ) -> Html msg
pick =
    Zipper.map (Tuple.pair False)
        >> Zipper.mapFocus (Tuple.mapFirst not)
        >> Zipper.flat
        >> List.map radio
        >> fieldset [ class "pick" ]


pickOrNot : Bool -> Zipper ( String, msg ) -> Html msg
pickOrNot isActive =
    Zipper.map (Tuple.pair False)
        >> (if isActive then
                Zipper.mapFocus (Tuple.mapFirst not)

            else
                identity
           )
        >> Zipper.flat
        >> List.map radio
        >> fieldset [ class "pickOrNot" ]


radio : ( Bool, ( String, msg ) ) -> Html msg
radio ( isOn, ( name, msg ) ) =
    label []
        [ input [ type_ "radio", Attributes.checked isOn, onClick msg ] []
        , span [] [ text name ]
        ]
