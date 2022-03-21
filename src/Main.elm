port module Main exposing (..)

import Accordion exposing (Accordion)
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Layout exposing (..)
import Url exposing (Url)


port pleaseCenter : String -> Cmd msg
port pleaseConfirm : String -> Cmd msg


type alias Model =
    { key : Nav.Key
    , url : Url
    , accordion : Accordion Msg
    }


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url key ->
                let
                    initialAccordion =
                        Accordion.site

                    initialModel =
                        { key = key
                        , url = initialUrl
                        , accordion = initialAccordion
                        }

                    initialUrl =
                        { url | fragment = Just (Accordion.location initialAccordion) }
                in
                initialModel
                    |> (case Debug.log "initialize with fragment" url.fragment of
                            Nothing ->
                                update (LinkClicked (Browser.Internal initialUrl))

                            Just _ ->
                                update (UrlChanged url)
                       )
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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( if url == model.url then
                { model | accordion = Accordion.flip model.accordion }

              else
                model
            , Nav.pushUrl model.key (Url.toString url)
            )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                newModel =
                    if Debug.log "new url" url /= Debug.log "current url" model.url then
                        { model | accordion = Accordion.find url model.accordion, url = url }

                    else
                        model
            in
            ( newModel
            , Cmd.batch 
                [Accordion.focus newModel.accordion |> Debug.log "centering in viewport" |> pleaseCenter
                , Accordion.location newModel.accordion |> Debug.log "confirming" |> pleaseConfirm
                ]
            )


{-| -}
view model =
    { title = "Moving across Thresholds"
    , body =
        [ Layout.typography
        , Html.hr [] []
        , Html.div [] [ Accordion.view model.accordion ]
        , Html.hr [] []
        , section
            [ header "" "example" "Fatigue as creative proposition"
            , p "This is the new Moving Across Thresholds website. Right now, you can’t see anything yet. This week, I’ll create the prototype, and a link to test it will appear here."
            , h2 "This Subheading is weirdäö@%&äÄ'"
            , dense "For more concrete discussion of content and structure, check out these collaborative docs."
            , p "This is the new Moving Across Thresholds website. Right now, you can’t see anything yet. This week, I’ll create the prototype, and a link to test it will appear here."
            , p "This is the new Moving Across Thresholds website. Right now, you can’t see anything yet. This week, I’ll create the prototype, and a link to test it will appear here."
            ]
        , Html.div
            [ css
                [ displayFlex
                , position relative
                , width (vw 100)
                , height (vh 100)
                , backgroundColor (rgb 100 200 200)
                ]
            ]
            [ Accordion.anarchiveX ]
        , Html.div
            [ css
                [ displayFlex
                , position relative
                , width (vw 100)
                , height (vh 100)
                , backgroundColor (rgb 100 200 200)
                ]
            ]
            [ Accordion.vimeoX ]
        ]
            |> List.map Html.toUnstyled
    }
