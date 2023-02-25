module Snippets.About exposing (contact, events, mat, team)

import Article
import Directory
import Html.String exposing (..)
import Html.String.Attributes exposing (..)
import Layout
import Snippet exposing (cacheImg)


mat : Article.BodyTemplate
mat =
    [ "Moving across Thresholds (MaT) is an event series that explores how the perceived limits or edges within experience - whether physical, structural or otherwise - can be approached as thresholds of potential that invite us to think and move in different ways. At MaT humans and nonhumans research thresholds as generative places of encounter where entities such as people, gasses and entire ecosystems can interact."
    , "Conceived as an experimental space, participants explore with full physical commitment what is foregrounded and what is pushed into the background. This is not accidental, but the result of historical processes, dominant pedagogical practices as well as systematic relations of power and oppression. Curated by dance artist Renae Shadler, MaT events combine movement, philosophy and activism, bringing together artists and researchers from neurodiverse and international backgrounds (Europe, Africa and the Global South). Together we ask:"
    , "What happens when the background comes to life and makes its presence felt?"
    , "What if the landscape becomes a part of us, just as we are a part of it?"
    , "What are the invisible thresholds of power?"
    , "What if we centre the periphery?"
    , "Since 2020 the MaT series has been hosting regular hybrid events at bUM in Kreuzberg, Berlin and online within an interactive Gathertown world where each participant can move freely with their avatar in a textured landscape, interact with those close to them through video and voice chat as well as participate in the creation of online/offline documents."
    , "Bespoke festival events include: a two-day event in April 2022 as part of the ‘SENSE’ series at Radialsystem in Berlin (DE) focusing on geographies of perception, and in November 2022 entitled ‘Tidal Shifts’ at Perform[d]ance in the regional city of Stralsund (DE)."
    ]
        |> List.map (text >> List.singleton >> p [])
        |> article [ class "richtext" ]
        |> always
        |> Article.Content (Just "Moving across Thresholds")


events : Article.BodyTemplate
events =
    [ "MaT aims to provide a democratic and safe space where all material generated is open-source. Whether participants join for one or more events, whatever they discover, they are encouraged to take it and explore it further. All events are free and attempt to be suitable for all bodies. "
    , "The interactive MaT archive includes: open documents, full-length videos of past events, a library of worded-companions and commissioned essays by past facilitators."
    ]
        |> List.map (text >> List.singleton >> p [])
        |> (++)
            [ h2 [] [ text "Festivals" ]
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
        |> article [ class "richtext" ]
        |> always
        |> Article.Content (Just "\u{00A0}")


team : Article.BodyTemplate
team =
    let
        closeArtist who dir =
            Directory.getClosestBy (String.length who // 4) who dir
                |> Maybe.map
                    (\uuid -> a [ href uuid ] [ text ("☛ " ++ who |> String.replace " " "\u{00A0}") ])
                |> Maybe.withDefault (text who)
    in
    (\dir ->
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
                        li [] [ span [ class "role" ] [ text role ], text ":\u{00A0}", closeArtist person dir ]
                    )
                |> ul []
            , hr [] []
            , small [] [ text "Presented by Renae Shadler & Collaborators. Supported by the NATIONAL PERFORMANCE NETWORK – STEPPING OUT, sponsored by the Federal Government Commissioner for Culture and Media within the framework of the initiative NEUSTART KULTUR, Aid Program Dance. 2020/21 activities with additional support from Marten Bequest Theater Fellowship (Perpetual Trust, Australia).\n" ]
            ]
    )
        |> Article.Content (Just "Team")


contact : Article.BodyTemplate
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
        |> always
        |> Article.Content (Just "Contact")
