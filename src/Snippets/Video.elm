module Snippets.Video exposing (..)

import Accordion.Segment as Segment
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


vimeoVideo : Int -> Html msg
vimeoVideo number =
    div [ attribute "style" "padding:56.25% 0 0 0;position:relative;" ]
        [ iframe
            [ attribute "allow" "autoplay; fullscreen; picture-in-picture"
            , attribute "dnt" "True"
            , attribute "controls" "False"
            , attribute "allowfullscreen" ""
            , attribute "frameborder" "0"
            , src <| "https://player.vimeo.com/video/" ++ String.fromInt number ++ "?h=39fbc4dbc1&color=70f0a0&title=0&byline=0&portrait=0&dnt=1"
            , attribute "style" "position:absolute;top:0;left:0;width:100%;height:100%;"
            ]
            []
        ]


trailers : Segment.BodyTemplate
trailers =
    let
        makeTrailer =
            \( h, number ) ->
                li [] [ vimeoVideo number, h2 [] [ text h ] ]

        hVideo =
            714389952
    in
    article []
        [ makeTrailer ( "Trailer", 688293718 )
        , [ ( "Series 1", 510475030 )
          , ( "Series 2", 544616520 )
          , ( "Series 3", 572540457 )
          , ( "Radialsystem", 685421693 )
          ]
            |> List.map makeTrailer
            |> ul [ class "video-carousel" ]
        ]
        |> Segment.Content (Just "Trailer")


videochannel : Segment.BodyTemplate
videochannel =
    article []
        [ [ 514927927
          , 533845894
          , 517426338
          , 581244925
          , 525617567
          , 537057257
          , 539421233
          , 549047696
          , 558664865
          , 564303449
          , 687420793
          ]
            |> List.map
                (\number ->
                    li [] [ vimeoVideo number ]
                )
            |> ul [ class "video-carousel dense" ]
        ]
        |> Segment.Content Nothing
