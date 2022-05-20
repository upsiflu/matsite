port module Main exposing (..)

import Accordion exposing (Accordion)
import Browser
import Browser.Navigation as Nav
import Codec exposing (Codec, decoder, encoder)
import Css exposing (..)
import Data
import Html as Unstyled
import Html.Attributes as UnstyledAttributes
import Html.Events as Events
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (href)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout exposing (..)
import Task
import Time
import TimeZone
import Ui
import Url exposing (Url)
import Url.Parser as UrlParser exposing (Parser)


port pleaseCenter : String -> Cmd msg


port pleaseConfirm : String -> Cmd msg


type alias SessionId =
    String


type Model
    = Loading
        { key : Nav.Key
        , url : Url
        , overwrite : Accordion.History
        }
    | Model
        { key : Nav.Key
        , url : Url
        , accordion : Accordion
        , backlog : Maybe Accordion.Intent
        , overwrite : Accordion.History
        , zone : ( String, Time.Zone )
        }


overwrite : Bool
overwrite =
    False


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url key ->
                let
                    initialModel =
                        Loading
                            { key = key
                            , url = url
                            , overwrite = Data.initialIntents
                            }
                in
                initialModel
                    |> update (UrlChanged url)
                    |> (\( model, cmd ) -> ( model, Cmd.batch [ cmd, TimeZone.getZone |> Task.attempt ZoneReceived ] ))
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



---- Update ----


type
    Msg
    -- Navigation
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- Client View
    | ZoneReceived (Result TimeZone.Error ( String, Time.Zone ))
    | ScrolledTo String
      -- Volatile Data
    | AccordionMessageReceived Accordion.Msg
      -- Persistent Data
    | IntentGenerated Accordion.Intent
    | LogReceived Accordion.History
    | NoteReceived String


type Route
    = Home
    | Segment String


route : Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Segment UrlParser.string
        ]


destination : Url -> String
destination url =
    case UrlParser.parse route url of
        Just Home ->
            ""

        Just (Segment s) ->
            s

        _ ->
            ""


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ---- Navigation
        ( LinkClicked (Browser.Internal url), Model m ) ->
            (\x ->
                {- Debug.log ("in Internal LinkClicked; path went from `" ++ destination m.url ++ "` to ") -}
                destination url |> (\_ -> x)
            )
            <|
                -- TODO: handle query and fragment
                if destination url == Accordion.parentId m.accordion && url.query == Nothing then
                    ( Model { m | accordion = Accordion.exit m.accordion }, Cmd.none )

                else
                    ( Model m, Url.toString url |> Nav.pushUrl m.key )

        ( LinkClicked (Browser.External href), Model m ) ->
            ( Model m, Nav.load href )

        ( UrlChanged url, Model m ) ->
            -- TODO: handle query and fragment
            (\x ->
                {- Debug.log ("in UrlChanged; path went from " ++ destination m.url ++ " to ") -}
                destination url |> (\_ -> x)
            )
            <|
                if destination url == Accordion.parentId m.accordion then
                    (\x ->
                        {- Debug.log "New destination equals current parentId" -}
                        destination url |> (\_ -> x)
                    )
                    <|
                        ( Model { m | url = url }, Cmd.none )

                else
                    (\x ->
                        {- Debug.log ("New destination" ++ destination url ++ "/=") -}
                        Accordion.parentId m.accordion |> (\_ -> x)
                    )
                    <|
                        let
                            newAccordion =
                                Accordion.goToParentId (destination url) m.accordion
                        in
                        ( Model { m | url = url, accordion = newAccordion }
                        , Cmd.batch
                            [ Accordion.focusId newAccordion |> pleaseCenter
                            , Accordion.parentId newAccordion |> pleaseConfirm
                            ]
                        )

        ---- Client View
        ( ZoneReceived result, Loading o ) ->
            case result of
                Ok ( str, z ) ->
                    let
                        initialAccordion =
                            Data.initial z
                                |> Accordion.goToParentId (destination o.url)

                        newModel =
                            Model
                                { key = o.key
                                , url = o.url
                                , accordion = initialAccordion
                                , backlog = Nothing
                                , overwrite = o.overwrite
                                , zone = ( str, z )
                                }
                    in
                    ( newModel, Accordion.focusId initialAccordion |> pleaseCenter )

                Err error ->
                    {- Debug.log "Zone Error" -}
                    error
                        |> (\_ -> ( model, Cmd.none ))

        ( ScrolledTo id, Model m ) ->
            ( Model { m | accordion = Accordion.goToId id m.accordion }, Cmd.none )

        ---- Volatile Data
        ( AccordionMessageReceived accMsg, Model m ) ->
            ( Model { m | accordion = Accordion.update accMsg m.accordion }, Cmd.none )

        ---- Persistent Data
        ( LogReceived log, Model m ) ->
            ( Model { m | accordion = Accordion.reviseHistory log m.accordion }, Cmd.none )

        ( IntentGenerated intent, Model m ) ->
            ( Model { m | backlog = Just intent }, Cmd.none )

        ( NoteReceived str, Model m ) ->
            {- Debug.log "NOTE RECEIVED" -}
            str
                |> (\_ -> ( model, Cmd.none ))

        _ ->
            ( model, Cmd.none )


{-| -}
view model =
    let
        viewAccordion m =
            Accordion.view
                { zone = Just m.zone, do = (|>) "initialSession" >> IntentGenerated, volatile = AccordionMessageReceived }
                m.accordion
                |> Ui.composeScenes
                    (Keyed.ul [ Attributes.class "overflow" ] >> Tuple.pair "overflow")
    in
    { title = "Moving across Thresholds"
    , body =
        [ Layout.typography
            |> Html.toUnstyled

        -- , Html.hr [] []
        , case model of
            Loading _ ->
                Html.text "Loading" |> Html.toUnstyled

            Model m ->
                Ui.view (viewAccordion m) |> Html.toUnstyled
        , Unstyled.div [] <|
            case model of
                Loading _ ->
                    []

                Model m ->
                    [ m.backlog
                        |> Maybe.map
                            (encoder Accordion.intentCodec
                                >> Encode.encode 0
                                >> UnstyledAttributes.attribute "backlog"
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
                        |> (++)
                            (if overwrite then
                                [ encoder Accordion.historyCodec m.overwrite
                                    |> Encode.encode 0
                                    |> UnstyledAttributes.attribute "overwrite"
                                ]

                             else
                                []
                            )
                        |> (++)
                            [ Events.on "e" (Decode.at [ "detail" ] Decode.string |> Decode.map NoteReceived) ]
                        |> (++)
                            [ Decode.at [ "detail" ] (decoder Accordion.historyCodec)
                                |> Decode.map LogReceived
                                |> Events.on "logReceived"
                            ]
                        |> Unstyled.node "append-log"
                        |> (|>) []
                    , [ Decode.at [ "detail" ] Decode.string
                            |> Decode.map ScrolledTo
                            |> Events.on "scrolledToA"
                      ]
                        |> Unstyled.node "closest-aisle"
                        |> (|>) []
                    ]
        ]
    }
