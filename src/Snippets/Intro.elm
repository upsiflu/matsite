module Snippets.Intro exposing (..)

import Accordion.Article as Article
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


intro : Article.BodyTemplate
intro =
    div [ attribute "class" "intro" ]
        [ node "vimeo-video" [] []

        {- div [ attribute "style" "padding:56.25% 0 0 0;position:relative;" ]
           [ iframe
               [ id "intro-player"
               , attribute "allow" "autoplay; fullscreen; picture-in-picture"
               , attribute "dnt" "true"
               , attribute "controls" "false"
               , attribute "allowfullscreen" ""
               , attribute "frameborder" "0"
               , src "https://player.vimeo.com/video/688293718?h=39fbc4dbc1&color=70f0a0&title=0&byline=0&portrait=0&dnt=1"
               , attribute "style" "position:absolute;top:0;left:0;width:100%;height:100%;"
               ]
               []
           ]
        -}
        ]
        |> Article.Illustration
