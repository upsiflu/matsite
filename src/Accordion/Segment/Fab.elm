module Accordion.Segment.Fab exposing
    ( Fab(..)
    , codec
    , merge
    , view, edit
    , default
    )

{-| Functionality of a Floating Action Button (Google terminology)
that manages its own data and can be queried but is stateless

@docs Fab
@docs codec


# Combine

@docs merge


# View

@docs view, edit

-}

import Codec exposing (Codec, bool, field, float, int, maybeField, string, variant0, variant1, variant2)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Occurrence exposing (Occurrence)
import Time
import Ui
import Zipper


{-| -}
type Fab
    = Register { link : String, occurrence : Occurrence }
    | Subscribe { link : String }


{-| -}
codec : Codec Fab
codec =
    Codec.custom
        (\reg sub value ->
            case value of
                Register r ->
                    reg r

                Subscribe s ->
                    sub s
        )
        |> Codec.variant1 "Register"
            Register
            (Codec.object (\l o -> { link = l, occurrence = o })
                |> Codec.field "link" .link string
                |> Codec.field "occurrence" .occurrence Occurrence.codec
                |> Codec.buildObject
            )
        |> Codec.variant1 "Subscribe"
            Subscribe
            (Codec.object (\l -> { link = l })
                |> Codec.field "link" .link string
                |> Codec.buildObject
            )
        |> Codec.buildCustom


type alias Record_link_String_occurrence_Occurrence_ =
    { link : String, occurrence : Occurrence }


default : String -> Fab
default =
    stringToFabType >> Maybe.withDefault (Subscribe { link = "https://" })


type alias Record_link_String_ =
    { link : String }



---- Combine


{-| according to the following rules:

1.  Events (`Register`) trump `Subscribe`
2.  Next `Subscribe` overwrites previous `Subscribe`
3.  Next `Event` merges with previous `Event`

-}
merge : Fab -> Fab -> Fab
merge next prev =
    case ( next, prev ) of
        ( Register r, Subscribe _ ) ->
            Register r

        ( Subscribe _, Register r ) ->
            Register r

        ( Subscribe s, Subscribe _ ) ->
            Subscribe s

        ( Register r0, Register r1 ) ->
            Register { r0 | occurrence = Occurrence.merge r0.occurrence r1.occurrence }



---- View


fabTypeToString : Fab -> String
fabTypeToString fab =
    case fab of
        Register _ ->
            "Register"

        Subscribe _ ->
            "Subscribe"


stringToFabType : String -> Maybe Fab
stringToFabType str =
    case str of
        "Register" ->
            Register { link = "https://", occurrence = [] } |> Just

        "Subscribe" ->
            Subscribe { link = "https://" } |> Just

        _ ->
            Nothing


{-| -}
edit : { a | zone : Maybe ( String, Time.Zone ), save : Maybe Fab -> msg } -> Maybe Fab -> Html msg
edit { zone, save } maybeFab =
    let
        editor =
            case maybeFab of
                Nothing ->
                    Html.text ""

                Just (Register ({ link, occurrence } as r)) ->
                    Html.fieldset [ class "ui" ]
                        [ Html.input [ class "link", title "Weblink (http://...)", type_ "input", value link, onInput (\l -> Register { r | link = l } |> Just |> save) ] []
                        , case zone of
                            Nothing ->
                                Html.text "Determining local time zone..."

                            Just z ->
                                Occurrence.edit { zone = z, save = \o -> Register { r | occurrence = o } |> Just |> save } occurrence
                        ]

                Just (Subscribe { link }) ->
                    Html.fieldset [ class "ui" ]
                        [ Html.input [ class "link", title "Weblink (http://...)", type_ "input", value link, (\l -> Subscribe { link = l }) >> Just >> save |> onInput ] []
                        ]
    in
    [ ( "Register", stringToFabType "Register" |> save )
    ]
        |> Zipper.create
            ( "Subscribe", stringToFabType "Subscribe" |> save )
            []
        |> (case maybeFab of
                Nothing ->
                    Ui.pickOrNot False

                Just fab ->
                    Zipper.findClosest (Tuple.first >> (==) (fabTypeToString fab))
                        >> Zipper.mapFocus (\( str, _ ) -> ( str ++ " ×", save Nothing ))
                        >> Ui.pickOrNot True
           )
        |> (\picker -> [ picker, editor ])
        |> Html.fieldset [ class "ui" ]



-- Edit the link, and, in the case of Register, add an Occurrence widget.


{-| -}
view : Fab -> Html Never
view fab =
    case fab of
        Register { link } ->
            Html.a [ class "register fab", href link ] [ Html.span [] [ Html.text "register" ] ]

        Subscribe { link } ->
            Html.a [ class "subscribe fab", href link ] [ Html.span [] [ Html.text "register" ] ]
