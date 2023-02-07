module Main exposing (Model(..), Msg(..), SessionId, main, overwrite, update, upgradeIfPossible, view)

import Accordion exposing (Accordion)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Codec exposing (decoder, encoder)
import Css exposing (..)
import Data
import Html as Unstyled
import Html.Attributes as UnstyledAttributes
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Layout
import Restrictive
import Restrictive.Layout
import Restrictive.Layout.Region exposing (Aspect)
import Restrictive.Ui as Ui exposing (Ui)
import Result.Extra as Result
import Task
import Time
import TimeZone
import Tuple exposing (pair)
import Url exposing (Url)
import Url.Codec exposing (Codec)
import Url.Parser as UrlParser exposing (Parser)


type alias SessionId =
    String


type Model
    = Loading
        { maybeZone : Maybe ( String, Time.Zone )
        , maybeNow : Maybe Time.Posix
        }
    | Model
        { accordion : Accordion
        , backlog : Maybe Accordion.Intent
        , overwrite : Accordion.History
        , zone : ( String, Time.Zone )
        , now : Time.Posix
        }


overwrite : Bool
overwrite =
    True


main : Restrictive.Application Model Msg
main =
    Restrictive.styledApplication
        { init =
            ( Loading
                { maybeNow = Nothing
                , maybeZone = Nothing
                }
            , Cmd.batch
                [ TimeZone.getZone |> Task.attempt ZoneReceived
                , Time.now |> Task.attempt NowReceived
                ]
            )
        , view = view
        , update = update
        }



{- init =
       \_ url key ->
           let
               initialModel : Model
               initialModel =
                   Loading
                       { key = key
                       , url = url
                       , maybeNow = Nothing
                       , maybeZone = Nothing
                       }
           in
           initialModel
               |> update (UrlChanged url)
               |> (\( model, cmd ) -> ( ( url, model ), Cmd.batch [ cmd, TimeZone.getZone |> Task.attempt ZoneReceived, Time.now |> Task.attempt NowReceived ] ))
   , view = \( url, model ) -> view model
   , update = \msg ( url, model ) -> update msg model |> Tuple.mapFirst (pair (Ui.update url))
   , subscriptions = \_ -> Sub.none
   , onUrlChange = UrlChanged
   , onUrlRequest = LinkClicked
-}
---- Update ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- Client View
    | ZoneReceived (Result TimeZone.Error ( String, Time.Zone ))
    | NowReceived (Result () Time.Posix)
    | ScrolledTo String
    | ScrolledIntoNowhere
      -- Volatile Data
    | AccordionMessageReceived Accordion.Msg
      -- Persistent Data
    | IntentGenerated Accordion.Intent
    | LogReceived Accordion.History


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ---- Navigation
        ( ScrolledTo id, Model m ) ->
            -- TODO: Change Url path instead (Cmd)
            ( Model { m | accordion = Accordion.goToId id m.accordion }, Cmd.none )

        ---- Client View
        ( ZoneReceived result, Loading o ) ->
            case result of
                Ok zone ->
                    Loading { o | maybeZone = Just zone }
                        |> upgradeIfPossible

                Err error ->
                    {- Debug.log "Zone Error" -}
                    error
                        |> (\_ -> ( model, Cmd.none ))

        ( NowReceived result, Loading o ) ->
            case result of
                Ok now ->
                    Loading { o | maybeNow = Just now }
                        |> upgradeIfPossible

                Err error ->
                    {- Debug.log "Now Error" -}
                    error
                        |> (\_ -> ( model, Cmd.none ))

        ---- Volatile Data
        ( AccordionMessageReceived accMsg, Model m ) ->
            ( Model { m | accordion = Accordion.update accMsg m.accordion }, Cmd.none )

        ---- Persistent Data
        ( LogReceived log, Model m ) ->
            ( Model { m | accordion = Accordion.reviseHistory log m.accordion }, Cmd.none )

        ( IntentGenerated intent, Model m ) ->
            ( Model { m | backlog = Just intent }, Cmd.none )

        _ ->
            ( model, Cmd.none )


upgradeIfPossible : Model -> ( Model, Cmd msg )
upgradeIfPossible model =
    Maybe.withDefault ( model, Cmd.none ) <|
        case model of
            Loading o ->
                Maybe.map2
                    (\(( _, z ) as zone) now ->
                        let
                            initialAccordion =
                                Data.initial z
                        in
                        ( Model
                            { accordion = initialAccordion
                            , backlog = Nothing
                            , overwrite = Data.initialIntents z
                            , zone = zone
                            , now = now
                            }
                        , Cmd.none
                        )
                    )
                    o.maybeZone
                    o.maybeNow

            _ ->
                Nothing


{-| -}
view : Model -> Restrictive.Document Aspect ( String, Html Msg )
view model =
    let
        viewAccordion : { m | zone : ( String, Time.Zone ), now : Time.Posix, accordion : Accordion } -> Ui Aspect ( String, Html Msg )
        viewAccordion m =
            Accordion.view
                { zone = m.zone
                , now = m.now
                , do = (|>) "initialSession" >> IntentGenerated
                , volatile = AccordionMessageReceived
                , scrolledTo = ScrolledTo
                , scrolledIntoNowhere = ScrolledIntoNowhere
                }
                m.accordion
    in
    { title = "Moving across Thresholds"
    , layout = Restrictive.Layout.noControl
    , body =
        (++) (Ui.html ( "typo", Layout.typography )) <|
            case model of
                Loading _ ->
                    Ui.html ( "Loading", Html.text "Loading" )

                Model m ->
                    (viewAccordion m
                        |> Ui.wrap (Keyed.ul [ Attributes.class "model" ] >> Tuple.pair "model" >> List.singleton)
                    )
                        ++ ([ m.backlog
                                |> Maybe.map
                                    (encoder Accordion.intentCodec
                                        >> Encode.encode 0
                                        >> Attributes.attribute "backlog"
                                        >> List.singleton
                                    )
                                |> Maybe.withDefault []
                                |> (++)
                                    (if overwrite then
                                        [ encoder Accordion.historyCodec m.overwrite
                                            |> Encode.encode 0
                                            |> Attributes.attribute "overwrite"
                                        ]

                                     else
                                        []
                                    )
                                |> (++)
                                    [ Decode.at [ "detail" ] (decoder Accordion.historyCodec)
                                        |> Decode.map LogReceived
                                        |> Events.on "logReceived"
                                    ]
                                |> Html.node "append-log"
                                |> (|>) []
                            ]
                                |> Html.div [ Attributes.class "database connection" ]
                                |> Tuple.pair "db"
                                |> Ui.html
                           )
    }
