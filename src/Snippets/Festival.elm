module Snippets.Festival exposing (Festival, collage, description, festivals, genericIllustration, radialsystemCredits, tidalShifts, tidalShifts2, tidalShiftsCollage, tidalShiftsVideo, video1, view)

import Article exposing (Article, BodyTemplate)
import Html.String exposing (..)
import Html.String.Attributes as Attr
import Layout
import Snippet exposing (cacheImg)


type alias Festival =
    { date : String
    , title : String
    , description : Article.BodyTemplate
    , collage : String
    , video : Maybe String
    }


festivals : List Festival
festivals =
    []


tidalShiftsVideo : BodyTemplate
tidalShiftsVideo =
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
                , Attr.src "https://player.vimeo.com/video/791948098?h=1f97d3c1c8&app_id=122963"
                , Attr.width 1280
                , Attr.height 720
                , Attr.attribute "frameborder" "0"
                , Attr.attribute "allow" "fullscreen; picture-in-picture"
                , Attr.attribute "allowfullscreen" ""
                , Attr.title "&#039;MaT - Foregrounding the background&#039; at Radialsystem 2022"
                , Attr.attribute "dnt" "true"
                ]
                []
            ]
        ]
        |> always
        |> Article.Illustration


tidalShifts : BodyTemplate
tidalShifts =
    div
        [ Attr.class "richtext"
        ]
        [ h2 []
            [ text "Tidal Shifts"
            ]
        , p []
            [ text """
            “Thresholds, like waves, are generative zones of potential.” --Ally Bisshop, 2022
            """ ]
        , p []
            [ text """Join us for a mini-festival of artist labs in the coastal town of Stralsund, where MaT will host four 3-hour labs that will take place both live at the Perform[d]ance studios and online in our customised Gathertown world. Together with guest facilitators from Brazil, Colombia and Israel this workshop weekend focuses on the meeting of land and sea as a site of change.""" ]
        , p []
            [ text """Conceived as an experimental space, participants explore with full physical commitment; Slow Transitions by Nattan Dobkin and how ‘mythical’ linear movements can become radical when done in slow motion, ritualistic choreographic processes led by Martha Hincapié Charry that look at the relationship of our bodies with planet earth, and ask ‘when was the last time you changed your mind?’ in a workshop led by Viviane Tabach where we share perspectives about sensitive topics.""" ]
        , p [] [ text "No dance experience necessary. Registration is free and you can register for one or more events by emailing your name and preferred workshop times to ", pre [] [ text "ticket@performdance.de" ] ]
        ]
        |> always
        |> Article.Content (Just "Moving across Thresholds:")


tidalShifts2 : BodyTemplate
tidalShifts2 =
    div
        [ Attr.class "richtext"
        ]
        [ h3 [] [ text "Guest Facilitators" ]
        , ul []
            [ li [] [ text "Nattan Dobkin - joining in person from Tel Aviv, Israel" ]
            , li [] [ text "Renae Shadler - joining in person from Berlin, Germany\n" ]
            , li [] [ text "Judith Förster - joining in person from Berlin, Germany" ]
            , li [] [ text "Martha Hincapié Charry - joining digitally" ]
            , li [] [ text "Viviane Tabach - joining digitally " ]
            ]
        ]
        |> always
        |> Article.Content (Just "\u{00A0}")


tidalShiftsCollage : BodyTemplate
tidalShiftsCollage =
    div
        []
        [ cacheImg "Tidal Shifts" 2 "" "https://lh4.googleusercontent.com/lxmaDpkMf9wfykNBjY2LswyaxOyhd_2ccu_tzVN-CoX3DuV9-8DLLbHTAyvDbA4pG4LZr3kU4r-O552wXhGBf4GLzX6JUyulqcdakuHCuBrtJtzLOyeZ9aEYgpF2FD5CCBhOkDa2pIsdQen34rY7UDCZTNGU4lrUspLK4rQMOB9qzX5wRdw4bNkBYw"
        ]
        |> always
        |> Article.Illustration


description : BodyTemplate
description =
    div
        [ Attr.class "richtext"
        ]
        [ h2 []
            [ text "Foregrounding the background"
            ]
        , p []
            [ text "The invisible thresholds of power are to be found in “Moving Across Thresholds” (MaT), an ongoing workshop format: choreographer Renae Shadler explores how the perceived limits or edges within experience – whether physical, structural or otherwise – can be approached as thresholds of potential that invite us to think and move in different ways. Conceived as an experimental space, participants explore with full physical commitment how limits can become thresholds to thought and action through broad engagement and active co-creation with diverse bodies in everyday life. The tried and tested format combines movement, philosophy and activism in a two-day intensive workshop as part of the “SENSE” series, through which Radialsystem explores power relations within a geography of perception." ]
        , p []
            [ text "The workshop at Radialsystem deals specifically with the topic of “Foregrounding the background”; that is, with the invisible thresholds that mark power and privilege in the relationship between foreground and background. What is highlighted and what is pushed into the background is not accidental, but the result of historical processes, dominant pedagogical practices as well as systematic relations of power and oppression. With international artists and researchers based in Berlin, São Paulo and Dar es salaam/Tanzania, the workshop reverses this relationship: what happens when the background comes to life and makes its presence felt? What if the landscape becomes a part of us, just as we are a part of it? What if we centre the periphery?" ]
        ]
        |> always
        |> Article.Content (Just "Moving across Thresholds:")


collage : BodyTemplate
collage =
    cacheImg "Moving across Thresholds at Radialsystem (Berlin) - Collage: Judith Förster" 2 "" "https://movingacrossthresholds.com/asset/festival/radial.png"
        |> always
        |> Article.Illustration


video1 : BodyTemplate
video1 =
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
                , Attr.src "https://player.vimeo.com/video/725722961?h=482dd66dac&app_id=122963"
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
        , div
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
        |> always
        |> Article.Illustration


radialsystemCredits : BodyTemplate
radialsystemCredits =
    let
        rolesAndNames =
            [ ( "Concept, Curation, Facilitation"
              , [ "Renae Shadler" ]
              )
            , ( "Guest Facilitators"
              , [ "Isack Abeneko"
                , "Giovana de Souza Possignolo"
                ]
              )
            , ( "Festival Companion"
              , [ "Judith Förster" ]
              )
            , ( "Communications"
              , [ "Flupsi" ]
              )
            , ( "Documentation"
              , [ "Stella Horta" ]
              )
            , ( "Production"
              , [ "Sofia Fantuzzi" ]
              )
            ]
                |> List.map
                    (\( role, people ) ->
                        li [ Attr.class "credit" ]
                            (span [ Attr.class "role" ]
                                [ text role ]
                                :: List.map (\who -> a [ Attr.href (Layout.sanitise who), Attr.class "who" ] [ text who ]) people
                            )
                    )
    in
    div [ Attr.class "richtext" ]
        [ ul [ Attr.class "credits" ] rolesAndNames ]
        |> always
        |> Article.Content (Just "Credits")


genericIllustration : BodyTemplate
genericIllustration =
    cacheImg "Festival (Illustration)" 2 "" "https://imgproxy.berlinonline.net/20OIHVWW_l8G4pdLCKj4_WO_cH6k6LtXAf6kHvbhxzM/pr:gallery/q:70/cb:2022032507/aHR0cHM6Ly9wb3B1bGEtbWlkZGxld2FyZS5zMy5hbWF6b25hd3MuY29tL2JvLW1pZGRsZXdhcmUvYm8uYmRlX2NoYW5uZWwuZXZlbnQvaW1hZ2VzLzk1L2U5ODJkZTE1LTNkZDUtZGIzNi1mNTVmLTNlOWNkMTc5ZDIxOS5qcGc.jpg"
        |> always
        |> Article.Illustration


view : Festival -> List Article
view _ =
    []
