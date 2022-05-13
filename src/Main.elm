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
import Url exposing (Url)


port pleaseCenter : String -> Cmd msg


port pleaseConfirm : String -> Cmd msg


type alias Model =
    { key : Nav.Key
    , url : Url
    , accordion : Accordion Msg
    , backlog : Backlog
    , zone : Maybe Time.Zone
    }


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
                        , backlog = ( 0, [] )
                        , zone = Nothing
                        }

                    initialUrl =
                        { url | fragment = Just (Accordion.parentId initialAccordion |> Debug.log "Initial Model location is") }
                in
                initialModel
                    |> (case Debug.log "Initialize with fragment" url.fragment of
                            Nothing ->
                                update (LinkClicked (Browser.Internal initialUrl))

                            Just _ ->
                                update (UrlChanged url)
                       )
                    |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Task.perform ZoneReceived Time.here ])
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
    | ZoneReceived Time.Zone
    | ActionGenerated Accordion.Action
    | ActionsReceived (List Accordion.Action)
    | AccordionMessageReceived Accordion.Msg
    | ScrolledTo String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ActionsReceived actions ->
            ( { model | accordion = Accordion.create actions |> Accordion.goToParentId model.url.fragment }, Cmd.none )

        ActionGenerated action ->
            ( { model | backlog = model.backlog |> (\( count, _ ) -> ( count + 1, [ Accordion.Find (Accordion.focusId model.accordion), action ] )) }, Cmd.none )

        AccordionMessageReceived accMsg ->
            ( { model | accordion = Accordion.update accMsg model.accordion }, Cmd.none )

        -- Send action via port to js!
        ZoneReceived zone ->
            ( { model | zone = Just zone }, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            let
                ( newUrl, newAccordion ) =
                    if Debug.log "Internal url clicked" url == model.url then
                        Accordion.exit model.accordion |> (\acc -> ( Accordion.parentId acc, acc ))

                    else
                        ( Url.toString url |> Debug.log "chosen new url", model.accordion )
            in
            ( { model | accordion = newAccordion }
            , if Accordion.isRoot model.accordion && newUrl == "" then
                Debug.log "is already root" Cmd.none

              else
                Nav.pushUrl model.key <| Debug.log "Need to change Url!" newUrl
            )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                newModel =
                    if Debug.log "Url changed to" url /= Debug.log "Url was previously" model.url then
                        { model | accordion = Accordion.goToParentId url.fragment model.accordion, url = url }

                    else
                        model
            in
            ( newModel
            , Cmd.batch
                [ Accordion.focusId newModel.accordion |> pleaseCenter
                , Accordion.parentId newModel.accordion |> pleaseConfirm
                ]
            )

        ScrolledTo id ->
            ( { model | accordion = Accordion.goToId id model.accordion }, Cmd.none )


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
            [ Unstyled.node "append-log" [ encodeBacklog model.backlog |> Encode.encode 0 |> UnstyledAttributes.attribute "backlog" ] []
            , Unstyled.node "closest-aisle"
                [ Events.on "scrolledToA" (Decode.map ScrolledTo (Decode.at [ "detail" ] Decode.string))
                ]
                []
            ]
        ]
    }


decodeBacklog : Decoder Backlog
decodeBacklog =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Decode.int)
        (Decode.field "A2" (Decode.list Accordion.decodeAction))


encodeBacklog : Backlog -> Value
encodeBacklog ( a1, a2 ) =
    Encode.object
        [ ( "A1", Encode.int a1 )
        , ( "A2", Encode.list Accordion.encodeAction a2 )
        ]


type alias Backlog =
    ( Int, List Accordion.Action )
