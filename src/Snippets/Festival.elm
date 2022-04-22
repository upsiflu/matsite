module Snippets.Festival exposing (..)

import Accordion.Segment as Segment
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)


type alias Festival =
    { date : String
    , title : String
    , description : Segment.Body Never
    , collage : String
    , video : Maybe String
    }


festivals : List Festival
festivals =
    [
        {

        }
    ]

description =
    div
        [ Attr.class "richtext"
        ]
        [ h2 []
            [ text "Moving Across Thresholds:"
            , br [] []
            , text "Foregrounding the background"
            ]
        , p []
            [ text "The invisible thresholds of power are to be found in “Moving Across Thresholds” (MaT), an ongoing workshop format: choreographer Renae Shadler explores how the perceived limits or edges within experience – whether physical, structural or otherwise – can be approached as thresholds of potential that invite us to think and move in different ways. Conceived as an experimental space, participants explore with full physical commitment how limits can become thresholds to thought and action through broad engagement and active co-creation with diverse bodies in everyday life. The tried and tested format combines movement, philosophy and activism in a two-day intensive workshop as part of the “SENSE” series, through which Radialsystem explores power relations within a geography of perception." ]
        , p []
            [ text "The workshop at Radialsystem deals specifically with the topic of “Foregrounding the background”; that is, with the invisible thresholds that mark power and privilege in the relationship between foreground and background. What is highlighted and what is pushed into the background is not accidental, but the result of historical processes, dominant pedagogical practices as well as systematic relations of power and oppression. With international artists and researchers based in Berlin, São Paulo and Dar es salaam/Tanzania, the workshop reverses this relationship: what happens when the background comes to life and makes its presence felt? What if the landscape becomes a part of us, just as we are a part of it? What if we centre the periphery?" ]
        ]
        |> Segment.Content


collage =
    img
        [ Attr.src "asset/radial(2).jpg"
        , Attr.alt "Collage"
        ]
        []
        |> Segment.Illustration


video =
    div [ Attr.class "bleeding" ]
        [ div
            [ Attr.class "TextformatterVideoEmbed"
            , Attr.style "position" "relative"
            , Attr.style "padding-bottom" "56.25%"
            , Attr.style "height" "0"
            , Attr.style "overflow" "hidden"
            ]
            [ iframe
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.attribute "byline" "false"
                , Attr.attribute "portrait" "false"
                , Attr.src "https://player.vimeo.com/video/685421693?h=482dd66dac&app_id=122963"
                , Attr.width 1280
                , Attr.height 720
                , Attr.attribute "frameborder" "0"
                , Attr.attribute "allow" "fullscreen; picture-in-picture"
                , Attr.attribute "allowfullscreen" ""
                , Attr.title "&#039;MaT - Foregrounding the background&#039; at Radialsystem 2022"

                -- , Attr.attribute "color" "50e678"
                , Attr.attribute "dnt" "true"
                ]
                []
            ]
        ]
        |> Segment.Illustration


view : Festival -> List (Segment)
view festival =
    []
    