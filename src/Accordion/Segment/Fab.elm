module Accordion.Segment.Fab exposing
    ( Fab(..)
    , codec
    , merge
    , view, edit
    , beginning, default, isActive
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


default : String -> Fab
default =
    stringToFabType >> Maybe.withDefault (Subscribe { link = "https://" })



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
                    []

                Just (Register ({ link, occurrence } as r)) ->
                    [ Html.input [ class "link", title "Weblink (http://...)", type_ "input", value link, onInput (\l -> Register { r | link = l } |> Just |> save) ] []
                    , case zone of
                        Nothing ->
                            Html.text "Determining local time zone..."

                        Just z ->
                            Occurrence.edit { zone = z, save = \o -> Register { r | occurrence = o } |> Just |> save } occurrence
                    ]

                Just (Subscribe { link }) ->
                    [ Html.input [ class "link", title "Weblink (http://...)", type_ "input", value link, (\l -> Subscribe { link = l }) >> Just >> save |> onInput ] [] ]
    in
    [ ( "Register", stringToFabType "Register" |> save )
    ]
        |> Zipper.create
            ( "Subscribe", stringToFabType "Subscribe" |> save )
            []
        |> (case maybeFab of
                Nothing ->
                    Tuple.pair False

                Just fab ->
                    Zipper.findClosest (Tuple.first >> (==) (fabTypeToString fab))
                        >> Zipper.mapFocus (\( str, _ ) -> ( str ++ " ×", save Nothing ))
                        >> Tuple.pair True
           )
        |> (\( active, options ) ->
                Zipper.map
                    (\( str, msg ) ->
                        ( { front = [ Html.text str ], title = "Select a Body type for this Segment" }
                        , Just msg
                        )
                    )
                    options
                    |> Ui.pickOrNot active
           )
        |> (\picker -> Html.legend [ class "no-break" ] [ Html.label [ class "ui" ] [ Html.text "Interactivity:\u{00A0}" ], picker ] :: editor)
        |> Html.fieldset [ class "ui" ]



-- Edit the link, and, in the case of Register, add an Occurrence widget.


{-| -}
view : { a | zone : ( String, Time.Zone ) } -> Fab -> Html Never
view { zone } fab =
    case fab of
        Register { link, occurrence } ->
            Html.a [ class "register fab", href link, title ("Upcoming: " ++ Occurrence.toString zone Occurrence.Minutes occurrence) ] [ Html.span [] [ Html.text "Register" ] ]

        Subscribe { link } ->
            Html.a [ class "subscribe fab", href link ] [ Html.span [] [ Html.text "Subscribe" ] ]


{-| -}
isActive : { c | now : Time.Posix } -> Fab -> Bool
isActive { now } fab =
    case fab of
        Subscribe _ ->
            True

        Register { occurrence } ->
            Occurrence.beginning occurrence
                |> Maybe.map (Time.posixToMillis >> (>=) (Time.posixToMillis now))
                |> Maybe.withDefault False


{-| -}
beginning : Fab -> Maybe Time.Posix
beginning fab =
    case fab of
        Subscribe _ ->
            Nothing

        Register { occurrence } ->
            Occurrence.beginning occurrence
