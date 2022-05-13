module Accordion.Segment.Fab exposing
    ( Fab(..)
    , decode, encode
    , merge
    , view, edit
    )

{-| Functionality of a Floating Action Button (Google terminology)
that manages its own data and can be queried but is stateless

@docs Fab
@docs decode, encode


# Combine

@docs merge


# View

@docs view, edit

-}

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Occurrence exposing (Occurrence)


{-| -}
type Fab
    = Register { link : String, occurrence : Occurrence }
    | Subscribe { link : String }


type alias Record_link_String_occurrence_Occurrence_ =
    { link : String, occurrence : Occurrence }


{-| -}
decode : Decoder Fab
decode =
    Decode.field "Constructor" Decode.string
        |> Decode.andThen
            (\constructor ->
                case constructor of
                    "Register" ->
                        Decode.map
                            Register
                            (Decode.field "A1" decodeRecord_link_String_occurrence_Occurrence_)

                    "Subscribe" ->
                        Decode.map
                            Subscribe
                            (Decode.field "A1" decodeRecord_link_String_)

                    other ->
                        Decode.fail <| "Unknown constructor for type Fab: " ++ other
            )


decodeRecord_link_String_ =
    Decode.map
        Record_link_String_
        (Decode.field "link" Decode.string)


decodeRecord_link_String_occurrence_Occurrence_ =
    Decode.map2
        Record_link_String_occurrence_Occurrence_
        (Decode.field "link" Decode.string)
        (Decode.field "occurrence" Occurrence.decode)


type alias Record_link_String_ =
    { link : String }


{-| -}
encode : Fab -> Value
encode a =
    case a of
        Register a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Register" )
                , ( "A1", encodeRecord_link_String_occurrence_Occurrence_ a1 )
                ]

        Subscribe a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Subscribe" )
                , ( "A1", encodeRecord_link_String_ a1 )
                ]


encodeRecord_link_String_ a =
    Encode.object
        [ ( "link", Encode.string a.link )
        ]


encodeRecord_link_String_occurrence_Occurrence_ a =
    Encode.object
        [ ( "link", Encode.string a.link )
        , ( "occurrence", Occurrence.encode a.occurrence )
        ]



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


{-| -}
edit : { save : Fab -> msg } -> Fab -> Html msg
edit { save } fab =
    Html.text "TODO Fab.edit"



-- Edit the link, and, in the case of Register, add an Occurrence widget.


{-| -}
view : Fab -> Html Never
view fab =
    case fab of
        Register { link } ->
            Html.a [ class "register fab", href link ] [ Html.span [] [ Html.text "register" ] ]

        Subscribe { link } ->
            Html.a [ class "subscribe fab", href link ] [ Html.span [] [ Html.text "register" ] ]
