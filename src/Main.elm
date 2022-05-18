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


type alias Model =
    { key : Nav.Key
    , url : Url
    , accordion : Accordion
    , backlog : Maybe Accordion.Intent
    , overwrite : Accordion.History
    , zone : Maybe ( String, Time.Zone )
    }


overwrite : Bool
overwrite =
    True


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url key ->
                let
                    initialAccordion =
                        Data.initial

                    initialModel =
                        { key = key
                        , url = initialUrl
                        , accordion = initialAccordion
                        , backlog = Nothing
                        , overwrite = Data.initialIntents
                        , zone = Nothing
                        }

                    initialUrl =
                        case url.path of
                            "/" ->
                                { url | path = Accordion.parentId initialAccordion }

                            _ ->
                                url
                in
                initialModel
                    |> update (UrlChanged initialUrl)
                    |> (\( model, cmd ) -> ( model, Cmd.batch [ cmd, TimeZone.getZone |> Task.attempt ZoneReceived, Accordion.focusId model.accordion |> pleaseCenter ] ))
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
    case msg of
        ---- Navigation
        LinkClicked (Browser.Internal url) ->
            (\x -> Debug.log ("in Internal LinkClicked; path went from " ++ destination model.url ++ " to ") (destination url) |> (\_ -> x)) <|
                -- TODO: handle query and fragment
                if destination url == Accordion.parentId model.accordion && url.query == Nothing then
                    ( { model | accordion = Accordion.exit model.accordion }, Cmd.none )

                else
                    ( model, Url.toString url |> Nav.pushUrl model.key )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            -- TODO: handle query and fragment
            (\x -> Debug.log ("in UrlChanged; path went from " ++ destination model.url ++ " to ") (destination url) |> (\_ -> x)) <|
                if destination url == Accordion.parentId model.accordion then
                    (\x -> Debug.log "New destination equals current parentId" (destination url) |> (\_ -> x)) <|
                        ( { model | url = url }, Cmd.none )

                else
                    (\x -> Debug.log ("New destination" ++ destination url ++ "/=") (Accordion.parentId model.accordion) |> (\_ -> x)) <|
                        let
                            newAccordion =
                                Accordion.goToParentId (destination url) model.accordion
                        in
                        ( { model | url = url, accordion = newAccordion }
                        , Cmd.batch
                            [ {- Accordion.focusId newAccordion |> pleaseCenter
                                 ,
                              -}
                              Accordion.parentId newAccordion |> pleaseConfirm
                            ]
                        )

        ---- Client View
        ZoneReceived result ->
            case result of
                Ok z ->
                    ( { model | zone = Just z }, Cmd.none )

                Err error ->
                    Debug.log "Zone Error" error
                        |> (\_ -> ( model, Cmd.none ))

        ScrolledTo id ->
            ( { model | accordion = Accordion.goToId id model.accordion }, Cmd.none )

        ---- Volatile Data
        AccordionMessageReceived accMsg ->
            ( { model | accordion = Accordion.update accMsg model.accordion }, Cmd.none )

        ---- Persistent Data
        LogReceived log ->
            ( { model | accordion = Accordion.reviseHistory log model.accordion }, Cmd.none )

        IntentGenerated intent ->
            ( { model | backlog = Just intent }, Cmd.none )

        NoteReceived str ->
            Debug.log "NOTE RECEIVED" str
                |> (\_ -> ( model, Cmd.none ))


{-| -}
view model =
    let
        accordion =
            Accordion.view
                { zone = model.zone, do = (|>) "initialSession" >> IntentGenerated, volatile = AccordionMessageReceived }
                model.accordion
                |> Ui.composeScenes
                    (Keyed.ul [ Attributes.class "overflow" ] >> Tuple.pair "overflow")
    in
    { title = "Moving across Thresholds"
    , body =
        [ Layout.typography
            |> Html.toUnstyled

        -- , Html.hr [] []
        , Ui.view accordion
            |> Html.toUnstyled
        , Unstyled.div []
            [ model.backlog
                |> Maybe.map
                    (encoder Accordion.intentCodec
                        >> Encode.encode 0
                        >> UnstyledAttributes.attribute "backlog"
                        >> List.singleton
                    )
                |> Maybe.withDefault []
                |> (++)
                    (if overwrite then
                        [ encoder Accordion.historyCodec model.overwrite
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
