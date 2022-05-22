module Snippets.About exposing (..)

import Accordion.Segment as Segment
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layout
import Ui exposing (cacheImg)


mat : Segment.BodyTemplate
mat =
    [ "(MaT) is an ongoing experience-based knowledge lab that combines movement, philosophy and inclusion. Together, we ask how we can perceive thresholds and lower them to encourage greater participation and active co-creation. 'Threshold' is a term that describes a barrier and the act of crossing it. It encompasses physical access requirements, language barriers and even the psychological threshold sometimes caused by the need to introduce oneself at the beginning of an event."
    , "The series is curated and often facilitated by choreographer Renae Shadler, with guests joining throughout the program. The lab is shaped by Renae’s research into choreographic thinking, embodied learning and attuning to more-than human worlds. "
    , "How can we conceive new forms of knowledge together that go beyond the linguistic? "
    , "How can we explore thoughts as experienceable and tangible materials?"
    , "What if the landscape becomes a part of us, just as we are a part of it?"
    , "What if we centre the periphery?"
    , "In 2022 MaT is hosting hybrid events simultaneously live at bUM in Kreuzberg/Berlin and online within our interactive Gathertown world. Each participant can move freely with their avatar in a textured landscape, interact with those close to them through video and voice chat as well as participate in the creation of online documents."
    , "The lab is a democratic and safe space where all material generated is shared collectively within the group. When you join, even if it’s only for one event, whatever you discover, we encourage you to take it, and explore it further."
    , "These are free events and suitable for all bodies. They take place every two weeks, from 20:00 to 21:30 (CET). Register is necessary."
    , "MaT also hosts festival weekends. In April there was a two-day lab as part of the “SENSE” series at Radialsystem in Berlin that explored invisible thresholds to power within a geography of perception, as well as a mini-festival in November initiated by MaT entitled 'Tidal Shifts' at Perform[d]ance in Stralsund. "
    ]
        |> List.map (text >> List.singleton >> p [])
        |> (\paragraphs ->
                paragraphs
                    ++ [ h2 [] [ text "Festivals" ]
                       , ul []
                            [ li [] [ a [ href <| Layout.sanitise "Radialsystem Berlin" ] [ text "Foregrounding the background" ] ]
                            , li [] [ a [ href <| Layout.sanitise "Perform[d]ance Stralsund" ] [ text "Tidal Shifts" ] ]
                            ]
                       , h2 [] [ text "Labs" ]
                       , ul [] <|
                            List.map
                                (\i ->
                                    li [] [ a [ href <| Layout.sanitise "Series" ++ String.fromInt i ] [ text <| "Series" ++ String.fromInt i ] ]
                                )
                                [ 1, 2, 3, 4, 5, 6 ]
                       ]
           )
        |> article []
        |> Segment.Content (Just "Moving across Thresholds")


team : Segment.BodyTemplate
team =
    article []
        [ cacheImg "Team" 1 "" "https://lh5.googleusercontent.com/DDQ5JPLeAEEx8DraEuc3NzLWq-u3CQl8alu-doc4KUBoBT6BizoBy3Cjs9RgJEoroC5-nH_cmI7VocfbgsmZaM5Y9GKhhizMb70OPpnSGk6IdbfxoFspHaq3_qPEa4T0c1V9YEZTrFpgp7DOHQ"
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
        [ cacheImg "Contact us" 1 "" "https://lh4.googleusercontent.com/Ds7cG-YcQPjFBkp5sKJv4wc6etScO-vQlpiHV9g4mmeWmKZZ8TntqJSciF9AzhooKkItlr3utsravVz4sKRS-d-LCpOV1INuWoiYq0AECHHD-mCqBNpnjoAig2RCo4SnN1YGzjyUlfJb8QQQRg"
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
        , p []
            [ text "To join our e-newsletter, send an email with 'Subscribe' in the subject to movingacrossthresholds@gmail.com"
            ]
        ]
        |> Segment.Content (Just "Contact")
