module Snippets.About exposing (..)

import Accordion.Segment as Segment
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layout
import Ui exposing (cacheImg)


mat : Segment.BodyTemplate
mat =
    [ "(MaT) is an ongoing experience-based knowledge lab that combines movement, philosophy and inclusion. Together, we ask how we can perceive thresholds and lower them to encourage greater participation and active co-creation. 'Threshold' is a term that describes a barrier and the act of crossing it. It encompasses physical access requirements, language barriers and even the psychological threshold sometimes caused by the need to introduce oneself at the beginning of an event."
    , "The series is curated and often facilitated by Renae Shadler, with guests joining throughout the program. The lab is shaped by Renae’s research into choreographic thinking, embodied learning and attuning to more-than human worlds. "
    , "How can we conceive new forms of knowledge together that go beyond the linguistic? "
    , "How can we explore thoughts as experienceable and tangible materials?"
    , "What if the landscape becomes a part of us, just as we are a part of it?"
    , "What if we centre the periphery?"
    , "In 2022 MaT will host hybrid events that are hosted live at bUM in Kreuzberg/Berlin and online within our interactive Gathertown world where each participant can move freely with their avatar in a textured landscape, interact with those close to them through video and voice chat as well as participate in the creation of online documents. This carries with it the new thresholds of simultaneously connecting both IRL (in-real-life) and URL (online)."
    , "The lab is a democratic and safe space where all material generated is shared collectively within the group. When you join, even if it’s only for one event, whatever you discover, we encourage you to take it, and explore it further."
    , "These are free events and suitable for all bodies. They take place every two weeks, from 20:00 to 21:30 (CET). Register to attend online or at bUM."
    ]
        |> List.map (text >> List.singleton >> p [])
        |> article []
        |> Segment.Content (Just "Moving across Thresholds")


team : Segment.BodyTemplate
team =
    article []
        [ cacheImg "Team" 1 "" "http://renaeshadler.com/wp-content/uploads/2021/06/thresh6.jpg"
        , [ ( "Concept, Curation, Facilitation", "Renae Shadler" )
          , ( "Creative Companion", "Susanne Schmitt" )
          , ( "Collages, Festival Companion", "Judith Förster" )
          , ( "Communications", "Flupsi Upsi" )
          , ( "Documentation", "Stella Horta" )
          , ( "Production", "Sofia Fantuzzi" )
          , ( "2021 Communications", "Katie-rose Spence" )
          , ( "2020/21 Production", "ehrliche arbeit – freelance office for culture" )
          ]
            |> List.map
                (\( role, person ) ->
                    li [] [ span [ class "role" ] [ text role ], a [ class "person", href (Layout.sanitise person) ] [ text person ] ]
                )
            |> ul []
        ]
        |> Segment.Content (Just "Team")


contact : Segment.BodyTemplate
contact =
    article []
        [ cacheImg "Contact us" 1 "" "http://renaeshadler.com/wp-content/uploads/2021/07/thresh-1.jpg"
        , [ ( "E-Mail", "mailto:movingAcrossThresholds@gmail.com" )
          , ( "Instagram page", "https://instagram.com/moving_across_thresholds/" )
          , ( "Website", "https://MovingAcrossThresholds.com" )
          , ( "Facebook", "https://facebook.com/renaeshadlerandco" )
          ]
            |> List.map
                (\( medium, link ) ->
                    li [] [ span [ class "medium" ] [ text ">>>" ], a [ class "medium", href link ] [ text medium ] ]
                )
            |> ul []
        ]
        |> Segment.Content (Just "Contact")
