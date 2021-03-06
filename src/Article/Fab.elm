module Article.Fab exposing
    ( Fab(..)
    , codec
    , default
    , merge
    , andUpcoming
    , occurrence
    , beginning, nextBeginning
    , isActive, isUpcoming
    , view, edit
    )

{-| Functionality of a Floating Action Button (Google terminology)
that manages its own data and can be queried but is stateless

@docs Fab
@docs codec


# Create

@docs default


# Combine

@docs merge


## Combine `Maybe Fab`

@docs andUpcoming


# Decompose

@docs occurrence
@docs beginning, nextBeginning
@docs isActive, isUpcoming


# View

@docs view, edit

-}

import Codec exposing (Codec, string)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import List.Extra as List
import Maybe.Extra as Maybe
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


{-| A `Subscribe` Fab
-}
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
edit : { a | zone : ( String, Time.Zone ), save : Maybe Fab -> msg } -> Maybe Fab -> Html msg
edit { zone, save } maybeFab =
    let
        editor =
            case maybeFab of
                Nothing ->
                    []

                Just (Register r) ->
                    [ Html.input [ class "link", title "Weblink (http://...)", type_ "input", value r.link, onInput (\l -> Register { r | link = l } |> Just |> save) ] []
                    , Occurrence.edit { zone = zone, save = \o -> Register { r | occurrence = o } |> Just |> save } r.occurrence
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
                        >> Zipper.mapFocus (\( str, _ ) -> ( str ++ " ??", save Nothing ))
                        >> Tuple.pair True
           )
        |> (\( active, options ) ->
                Zipper.map
                    (\( str, msg ) ->
                        ( { front = [ Html.text str ], title = "Select a Body type for this Article" }
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
        Register r ->
            Html.a [ class "register fab", target "_blank", href r.link, title ("Upcoming: " ++ Occurrence.toString zone Occurrence.Minutes r.occurrence) ] [ Html.span [ class "title" ] [ Html.text "Register" ] ]

        Subscribe _ ->
            Html.details
                [ class "subscribe fab" ]
                [ Html.summary [ title "Receive our e-newsletter!" ] [ Html.span [ class "title" ] [ Html.text "Subscribe" ] ]
                , Html.div [ class "subscribe display" ]
                    [ Html.h2 []
                        [ Html.text "Send an e-Mail with the subject line 'subscribe' to:"
                        ]
                    , Html.node "copy-text" [] []
                    ]
                ]


{-| where Nothing means something like eternal
-}
occurrence : Fab -> Maybe Occurrence
occurrence fab =
    case fab of
        Subscribe _ ->
            Nothing

        Register r ->
            Just r.occurrence


{-| This is True if either:
a. Starting time is now or in the future
b. No starting time is given (it\`s an eternal event or a subscribe fab)
-}
isActive : { c | now : Time.Posix } -> Fab -> Bool
isActive { now } =
    occurrence
        >> Maybe.andThen Occurrence.beginning
        >> Maybe.map (Time.posixToMillis >> (<=) (Time.posixToMillis now))
        >> Maybe.withDefault True


{-| This is True if:
any occasion starts now or later
-}
isUpcoming : { c | now : Time.Posix } -> Fab -> Bool
isUpcoming { now } =
    occurrence
        >> Maybe.unwrap False (List.any (.from >> Time.posixToMillis >> (<=) (Time.posixToMillis now)))


{-| filter: any occasion starts now or later
-}
andUpcoming : { c | now : Time.Posix } -> Maybe Fab -> Maybe Fab
andUpcoming =
    isUpcoming
        >> Maybe.filter


{-| Milliseconds at which the next-upcoming occasion starts
-}
nextBeginning : { c | now : Time.Posix } -> Fab -> Maybe Int
nextBeginning { now } =
    occurrence
        >> Maybe.andThen
            (List.filterMap (.from >> Time.posixToMillis >> Just >> Maybe.filter ((<=) (Time.posixToMillis now)))
                >> List.minimum
            )


{-| earliest time another occasion starts
-}
beginning : Fab -> Maybe Time.Posix
beginning =
    occurrence
        >> Maybe.andThen Occurrence.beginning
