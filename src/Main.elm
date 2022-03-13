module Main exposing (..)

import Browser
import Css exposing (..)
import Accordion
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Layout exposing (..)


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view >> Html.toUnstyled
        }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view model =
    Html.div []
        [ Layout.typography
        , Html.hr [] []
        , Html.div [] [ Accordion.view (Debug.log "site:" Accordion.site) ]
        , Html.hr [] []
        , section
            [ header "Fatigue as creative proposition"
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
