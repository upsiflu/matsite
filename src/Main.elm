port module Main exposing (..)

import Accordion exposing (Accordion)
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Data
import Html as Unstyled
import Html.Attributes as UnstyledAttributes
import Html.Events as Events
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (href)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout exposing (..)
import Task
import Time
import TimeZone
import Url exposing (Url)
import Url.Parser as UrlParser exposing (Parser)


port pleaseCenter : String -> Cmd msg


port pleaseConfirm : String -> Cmd msg


type alias Model =
    { key : Nav.Key
    , url : Url
    , accordion : Accordion Msg
    , backlog : Maybe Backlog
    , overwrite : List Backlog
    , zone : Maybe ( String, Time.Zone )
    }


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url key ->
                let
                    initialAccordion =
                        Data.initial

                    actionToBacklog i a =
                        ( i, "", a )

                    initialModel =
                        { key = key
                        , url = initialUrl
                        , accordion = initialAccordion
                        , backlog = Nothing
                        , overwrite = List.indexedMap actionToBacklog Data.initialActions
                        , zone = Nothing
                        }

                    initialUrl =
                        { url | path = Accordion.parentId initialAccordion |> Debug.log "Initial Model location is" }
                in
                initialModel
                    |> update (UrlChanged initialUrl)
                    |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, TimeZone.getZone |> Task.attempt ZoneReceived ])
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
    | ActionGenerated Accordion.Action
    | LogReceived (List Backlog)
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
                            [ Accordion.focusId newAccordion |> pleaseCenter
                            , Accordion.parentId newAccordion |> pleaseConfirm
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
            let
                actions =
                    List.map (\( _, _, a ) -> a) log
            in
            ( { model | accordion = Accordion.create actions |> Accordion.goToParentId (destination model.url) }, Cmd.none )

        ActionGenerated action ->
            let
                ordinal =
                    Maybe.map (\( o, _, _ ) -> o) model.backlog
                        |> Maybe.withDefault 0
            in
            ( { model | backlog = Just ( ordinal + 1, Accordion.focusId model.accordion, action ) }, Cmd.none )

        NoteReceived str ->
            Debug.log "NOTE RECEIVED" str
                |> (\_ -> ( model, Cmd.none ))


{-| -}
view model =
    { title = "Moving across Thresholds"
    , body =
        [ Layout.typography
            |> Html.toUnstyled

        -- , Html.hr [] []
        , Html.div [ Attributes.class "overflow" ]
            [ Accordion.view
                { zone = model.zone, do = ActionGenerated, volatile = AccordionMessageReceived }
                model.accordion
            ]
            |> Html.toUnstyled
        , Unstyled.div []
            [ model.backlog
                |> Maybe.map (encodeBacklog >> Encode.encode 0 >> UnstyledAttributes.attribute "backlog" >> List.singleton)
                |> Maybe.withDefault []
                |> (++)
                    [ Encode.list encodeBacklog model.overwrite |> Encode.encode 0 |> UnstyledAttributes.attribute "overwrite" ]
                |> (++)
                    [ Events.on "e" (Decode.at [ "detail" ] Decode.string |> Decode.map NoteReceived) ]
                |> (++)
                    [ Decode.at [ "detail" ] (Decode.list decodeBacklog)
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


decodeBacklog : Decoder Backlog
decodeBacklog =
    Decode.map3
        (\a1 a2 a3 -> ( a1, a2, a3 ))
        (Decode.field "Ordinal" Decode.int)
        (Decode.field "Location" Decode.string)
        (Decode.field "Action" Accordion.decodeAction)


encodeBacklog : Backlog -> Value
encodeBacklog ( ordinal, location, action ) =
    Encode.object
        [ ( "Ordinal", Encode.int ordinal )
        , ( "Location", Encode.string location )
        , ( "Action", Accordion.encodeAction action )
        ]


type alias Backlog =
    ( Int, String, Accordion.Action )
