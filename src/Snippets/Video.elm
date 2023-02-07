module Snippets.Video exposing (deferredVimeoVideo, videochannel, vimeoVideo)

import Article
import Directory exposing (Directory)
import Html.String exposing (..)
import Html.String.Attributes exposing (..)
import Layout
import Restrictive.Ui as Ui
import Snippet exposing (cacheImg)


vimeoVideo : Int -> Html msg
vimeoVideo number =
    div [ attribute "style" "padding:56.25% 0 0 0;position:relative;" ]
        [ div []
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
        ]


{-| -}
deferredVimeoVideo : String -> Int -> Html msg
deferredVimeoVideo hint number =
    details [ class "deferredVimeoVideo" ]
        [ summary []
            [ img [ title hint, src ("https://vumbnail.com/" ++ String.fromInt number ++ ".jpg") ] [] ]
        , div [ class "placeholder" ]
            [ img [ title hint, src ("https://vumbnail.com/" ++ String.fromInt number ++ ".jpg") ] [] ]
        , iframe
            [ attribute "allow" "autoplay; fullscreen; picture-in-picture"
            , attribute "dnt" "True"
            , attribute "autoplay" "1"
            , attribute "autopause" "1"
            , attribute "controls" "False"
            , attribute "allowfullscreen" ""
            , attribute "frameborder" "0"
            , src <| "https://player.vimeo.com/video/" ++ String.fromInt number ++ "?h=39fbc4dbc1&color=70f0a0&title=0&byline=0&portrait=0&dnt=1&autoplay=1&autopause=1"
            ]
            []
        ]


videochannel : Article.BodyTemplate
videochannel =
    let
        makeTrailer =
            \( h, number ) ->
                li [ class "trailer-thumb" ]
                    [ deferredVimeoVideo h number
                    , h2 []
                        [ text h
                        , text " "
                        , a [ href (Layout.sanitise h) ]
                            [ if h /= "" then
                                text "More..."

                              else
                                Snippet.none
                            ]
                        ]
                    ]
    in
    (\dir ->
        article []
            [ videocarousel dir
            , h2 [] [ text "Trailers" ]
            , makeTrailer ( "", 777887047 )
            , makeTrailer ( "", 688293718 )
            , [ ( "Series 1", 510475030 )
              , ( "Series 2", 544616520 )
              , ( "Series 3", 572540457 )
              , ( "Radialsystem", 685421693 )
              , ( "Perform[d]ance", 791948098 )
              ]
                |> List.map makeTrailer
                |> ul [ class "video-carousel" ]
            ]
    )
        |> Article.Content Nothing


seeLab : Directory.Directory -> String -> List (Html msg)
seeLab dir descr =
    Directory.getClosestBy (String.length descr // 4) descr dir
        |> Maybe.map
            (\uuid -> [ a [ class "seeLab", href (Layout.sanitise uuid) ] [ span [ class "lab-logo" ] [ text " " ], span [ class "hover-caption lab-title" ] [ text ("☛\u{00A0}" ++ descr ++ " ") ] ] ])
        |> Maybe.withDefault [ div [ class "seeNothing" ] [ span [ class "hover-caption other-title" ] [ text descr ] ] ]


videocarousel : Directory -> Html msg
videocarousel =
    \dir ->
        article []
            [ [ ( 514927927, "Erin Manning conversation" )
              , ( 533845894, "dances of the mouth" )
              , ( 517426338, "dances of\u{00A0}care" )
              , ( 581244925, "parallel worlds" )
              , ( 525617567, "creating a Verbal Climate" )
              , ( 537057257, "Botanizing the virtual" )
              , ( 539421233, "Wandering the city" )
              , ( 549047696, "layers of cells/earth/a.../?" )
              , ( 558664865, "Facing the\u{00A0}impossible" )
              , ( 564303449, "staying with the muddle" )
              , ( 687420793, "Glitching scores and code" )
              , ( 691848907, "Fatigue as creative proposition" )
              , ( 728908770, "Foregrounding the\u{00A0}Background" )
              , ( 744657368, "Konwn\u{00A0}to Unknown, Alienation" )
              , ( 737716811, "Learning to love the microbiome" )
              , ( 791948098, "Tidal Shifts" )
              , ( 789192195, "Scalar Sensorium" )
              , ( 788722195, "Hapticality\u{00A0}in a\u{00A0}different\u{00A0}light. Nighttime phytography and electric energy." )
              ]
                |> List.reverse
                |> List.map
                    (\( number, title ) ->
                        li [ class "hover-caption-on-hover" ] (deferredVimeoVideo title number :: seeLab dir title)
                    )
                |> ul [ class "video-carousel dense" ]
            ]
