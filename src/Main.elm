port module Main exposing (..)

import Accordion exposing (Accordion)
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Data
import Html as Unstyled
import Html.Attributes as UnstyledAttributes
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (href)
import Html.Styled.Keyed as Keyed
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
        , maybeZone : Maybe ( String, Time.Zone )
        , maybeNow : Maybe Time.Posix
        }
    | Model
        { key : Nav.Key
        , url : Url
        , accordion : Accordion
        , backlog : Maybe Accordion.Intent
        , overwrite : Accordion.History
        , zone : ( String, Time.Zone )
        , now : Time.Posix
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
                            , maybeNow = Nothing
                            , maybeZone = Nothing
                            }
                in
                initialModel
                    |> update (UrlChanged url)
                    |> (\( model, cmd ) -> ( model, Cmd.batch [ cmd, TimeZone.getZone |> Task.attempt ZoneReceived, Time.now |> Task.attempt NowReceived ] ))
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



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
    | NoteReceived String


type Route
    = Home
    | Article String


route : Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Article UrlParser.string
        ]


destination : Url -> String
destination url =
    case UrlParser.parse route url of
        Just Home ->
            ""

        Just (Article s) ->
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
                    ( Model { m | accordion = Accordion.exit m.accordion }, Accordion.parentId m.accordion |> pleaseCenter )

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

        ( ScrolledTo id, Model m ) ->
            ( Model { m | accordion = Accordion.goToId id m.accordion }, Cmd.none )

        ( ScrolledIntoNowhere, Model m ) ->
            ( Model m, Accordion.focusId m.accordion |> pleaseCenter )

        ---- Volatile Data
        ( AccordionMessageReceived accMsg, Model m ) ->
            ( Model { m | accordion = Accordion.update accMsg m.accordion }, Cmd.none )

        ---- Persistent Data
        ( LogReceived log, Model m ) ->
            ( Model { m | accordion = Accordion.reviseHistory log m.accordion }, Cmd.none )

        ( IntentGenerated intent, Model m ) ->
            ( Model { m | backlog = Just intent }, Cmd.none )

        ( NoteReceived str, _ ) ->
            {- Debug.log "NOTE RECEIVED" -}
            str
                |> (\_ -> ( model, Cmd.none ))

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
                                    |> Accordion.goToParentId (destination o.url)
                        in
                        ( Model
                            { key = o.key
                            , url = o.url
                            , accordion = initialAccordion
                            , backlog = Nothing
                            , overwrite = Data.initialIntents z
                            , zone = zone
                            , now = now
                            }
                        , Accordion.focusId initialAccordion |> pleaseCenter
                        )
                    )
                    o.maybeZone
                    o.maybeNow

            _ ->
                Nothing


{-| -}
view model =
    let
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
                |> Ui.composeScenes
                    (Keyed.node "li" [ Attributes.class "overflow" ] >> Tuple.pair "overflow")
    in
    { title = "Moving across Thresholds"
    , body =
        Html.toUnstyled Layout.typography
            :: (case model of
                    Loading _ ->
                        [ Html.text "Loading" |> Html.toUnstyled ]

                    Model m ->
                        [ Ui.view (viewAccordion m)
                            |> Keyed.ul [ Attributes.class "model" ]
                            |> Html.toUnstyled
                        , Unstyled.div [ UnstyledAttributes.class "database connection" ] <|
                            [{- m.backlog
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
                                ,
                             -}
                            ]
                        ]
               )
    }
