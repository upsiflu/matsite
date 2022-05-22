module Snippets.Series exposing (..)

import Accordion exposing (Action(..))
import Accordion.Segment as Segment exposing (InfoTemplate(..), Orientation(..), Shape(..))
import Accordion.Segment.Fab as Fab exposing (Fab)
import Accordion.Segment.ViewMode as ViewSegment
import Fold exposing (Direction(..))
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layout
import Levenshtein
import List.Extra as List
import Maybe.Extra as Maybe
import Occurrence exposing (Occurrence, Precision(..))
import Snippets.Festival exposing (Festival)
import Snippets.Lab exposing (Lab)
import Snippets.Video as Video
import Time exposing (Month(..))
import Ui exposing (cacheImg)


type Collage
    = Image Int String
    | Vimeo Int Int


collageColumns : Collage -> Int
collageColumns c =
    case c of
        Image cc _ ->
            cc

        Vimeo cc _ ->
            cc


collageToIllustration : Event -> List ( String, Segment.BodyTemplate )
collageToIllustration event =
    (\illu ->
        div []
            [ illu, Maybe.map (\d -> p [] [ text d ]) event.description |> Maybe.withDefault Ui.none ]
            |> Segment.Illustration
            |> Tuple.pair (event.title ++ " (Collage)")
            |> List.singleton
    )
    <|
        case event.collage of
            Just (Image _ str) ->
                cacheImg event.title 2 "" str

            Just (Vimeo _ int) ->
                Video.vimeoVideo int

            Nothing ->
                Ui.none


type alias Event =
    { day : Int, month : Month, year : Int, title : String, description : Maybe String, location : String, facilitator : String, country : Maybe String, collage : Maybe Collage }


type alias Series =
    { number : Int, motto : String, events : List Event }


allCaptions : List String
allCaptions =
    data
        |> List.concatMap
            (\s ->
                ("Series " ++ String.fromInt s.number)
                    :: List.map
                        .title
                        s.events
            )


closeEvent : Int -> String -> Maybe String
closeEvent proximity str =
    List.map (\caption -> ( Levenshtein.distance str caption, Layout.sanitise caption )) allCaptions
        |> List.minimumBy Tuple.first
        |> Maybe.filter (Tuple.first >> (>) proximity)
        |> Maybe.map Tuple.second


data =
    [ Series 1
        "LOWERING THE THRESHOLD"
        [ Event 8 Oct 20 "Erin Manning conversation" Nothing "bUM - Live event & Zoom" "Erin Manning" (Just "CA") Nothing
        , Event 17 Oct 20 "backwards walking brunch" Nothing "bUM - Live event" "Renae Shadler" Nothing Nothing
        , Event 23 Oct 20 "with infrasound / with me" Nothing "bUM - Live event" "Renae Shadler" Nothing Nothing
        , Event 6 Nov 20 "digital pond" Nothing "Zoom" "Renae Shadler" Nothing (Just <| Image 2 "https://lh4.googleusercontent.com/JWPD77B3ttRq9Ky9y-EvVtG6DxnI3J8K2eOi75cmeMfLBAkOnTa_mx8UQ8sbMe8N1NnLmBkPZP3-DJpxVNOKXmrpD8lv0gNmI0_Ymigz0U-Fwu3YJSRSM6K-MEdKhzq818eJ8PWegl1Pin7W-A")
        , Event 7 Nov 20 "middling brunch" Nothing "Zoom" "Renae Shadler" Nothing Nothing
        , Event 20 Nov 20 "internet imaginaries" Nothing "Zoom" "Renae Shadler" Nothing Nothing
        , Event 4 Dec 20 "dances of the mouth" Nothing "Zoom" "Mmakgosi Kgabi" (Just "BW/DE") Nothing
        , Event 12 Dec 20 "Access and Excess" Nothing "Zoom" "Balam Kenter" (Just "TR") Nothing
        , Event 18 Dec 20 "dances of care" Nothing "Zoom" "Sindri Runudde" (Just "SE") (Just <| Vimeo 3 517426338)
        ]
    , Series 2
        "ECOLOGY"
        [ Event 15 Jan 21 "parallel worlds" Nothing "Zoom" "Renae Shadler with Roland Walter" (Just "DE") Nothing
        , Event 23 Jan 21 "coming together and in relay" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 29 Jan 21 "drifting" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 12 Feb 21 "exploring the virtual" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 20 Feb 21 "Tender Hotel edition" Nothing "Zoom ** part of Tender Hotel festival" "Renae Shadler" Nothing Nothing
        , Event 26 Feb 21 "creating a Verbal Climate" Nothing "Gather-town" "Rivca Rubin" (Just "UK") Nothing
        , Event 12 Mar 21 "Botanizing the virtual" Nothing "Gather-town" "Susanne Schmitt" (Just "DE") Nothing
        , Event 20 Mar 21 "Wandering the City" Nothing "Gather-town & city walk" "Anna Mayberry" (Just "FR/DE") Nothing
        , Event 26 Mar 21 "the Falling Sky" Nothing "Gather-town" "Renae Shadler" Nothing (Just <| Image 2 "https://lh4.googleusercontent.com/856E1wbwscUqPRPoTMxb5vyHnmIzRIeSgPgu4aTuEjpfO7KSYK2nD6oKQoGa1tNg3qNmviODi_f0D8o45EES8suzctIU8v5qpryV90u1zgSCGP8vU2J8A9PvaY1McMJIDED-bgOMkc3an8YtSw")
        ]
    , Series 3
        "INTERRELATION"
        [ Event 9 Apr 21 "layers of cells/earth/a.../?" Nothing "Gather-town" "Cinzia Schincariol" (Just "IT") Nothing
        , Event 17 Apr 21 "building habitats and interconnections" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 23 Apr 21 "stream of emergent patters" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 7 May 21 "Nonhuman Sensations in Technoplanetary Layers" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 15 May 21 "Tentacular Thinking" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 21 May 21 "Facing the Impossible" Nothing "Gather-town" "Maikon K" (Just "BR") Nothing
        , Event 11 Jun 21 "staying with the muddle" Nothing "Gather-town" "Julia Grillmayr" (Just "AT") Nothing
        , Event 19 Jun 21 "imperfect intimacy" Nothing "Gather-town" "Renae Shadler" Nothing (Just <| Image 3 "https://lh5.googleusercontent.com/qTs0SsvN6DP0vnk1soKzE1hPboF1atG_iKvA_yAyHTZNGpU3qjT1cBmcn4wLTib2DzbEjI8cEaTu3p3KYp0TQDQnGjxLk-zVQdJEjPH4jqCJb4DqSmX6JcyzKB7CT7QS5-Qn1WYiqwxTGhO7PA")
        , Event 25 Jun 21 "social dreaming" Nothing "Gather-town" "Renae Shadler" Nothing Nothing
        ]
    , Series 4
        "EMERGENCE"
        [ Event 10 Feb 22 "hybrid landscapes" (Just "In 'hybrid landscapes' we explore how we can move together, while being physically apart. During the pandemic many of us have become increasingly distributed, screen-based and stationary. How can we now both physically and digitally come together to embrace embodied knowledge and explore the screen as a portal? Play with communality as synchrony? Find pleasure in the contours of our common hybrid landscape, a labyrinth to find each other within. Facilitated by choreographer/dancer - Renae Shadler together with cultural anthropologist - Dr. Susanne Schmitt, the group will create bodied contours, exploring where our data bodies and living bodies meet.") "Gather-town + bUm, Kreuzberg" "Renae Shadler and Susanne Schmitt" Nothing (Just <| Image 2 "https://lh4.googleusercontent.com/EKcHGCC5lVLiHbXVcduD-LRn8icZWURZ6iOLjsgjpbbbTfJs3Fih9sDLLsTT9dfo6EM2MYTH_bXdZqlEHDM5uzf5KNfxLgaPdjeAkie-VmDGHOP7WMDCl3p8gv1151AOskkoS4RYEei20w2X3A")
        , Event 24 Feb 22 "Glitching scores and code" (Just "How can the internet as a material expand - or ‘glitch’ - the construct of the body? With the ongoing aim to explore thresholds and the act of crossing them - ’Moving across Thresholds’ (MaT) focuses this event on ‘glitch’ as a vehicle to bring our online and IRL (in-real-life) imaginaries closer together. As hybrid beings do we have to be either/or? can we get glitched? embody our avatars? become cosmic? We will begin with a collective reading of ‘Elsewhere, After the Flood: Glitch Feminism and the Genesis of Glitch Body Politic’ by Legacy Russel where she proposes digitality as a mode of resistance against the body binaries of male/female, black/white, poor/rich and onwards. Inspired by Russel’s words we will translate our coded identities within our Gathertown world into movement scores that dance us.") "Gather-town + bUm, Kreuzberg" "Renae Shadler and Susanne Schmitt" Nothing (Just <| Image 2 "https://lh5.googleusercontent.com/JhJ1uj57uwAdnGRV_7lX80pjdnPrCIika0V5G8Xnzmt7fTVDIraOmwub2ADxMe1PCkoci2zpXLwAN_-1hzPgAw-dMvCr01oMFJSbAjc2nAXbIUVzVN7ZSLW6-Diq4E1VO5dEBiuRe6td2MB2eg")
        , Event 10 Mar 22 "Fatigue as creative proposition" (Just "The organ of vividness, which is also the organ of novelty and the organ of fatigue, has been atrophied. – Alfred North Whitehead.  As the pandemic threatens to become a kind of infinite loop, many of us are faced with overwhelming fatigue: physical and psychic exhaustion, a shrinking of the horizons of possibility. In this lab, facilitated by guest artist, writer and researcher Ally Bisshop, we will collectively explore fatigue – how it writes itself upon our bodies, how it frustrates and re-shapes our desires, how it textures and contours experience. If fatigue is felt as a deadening weight, an atrophying or wasting away of our ‘organ of vividness’, how can we seize something affirmative or generative from it? What kinds of techniques for relation, for movement, does fatigue propose?  Fatigue is not a singular quality of experience; it is a process; perhaps felt as a tension between Hypnos (sleep) and Eros (desire), or between repetition and creation; perhaps felt as a resistance against acceleration or verticality. Feeling into these thresholds of tension and resistance might offer ways of thinking fatigue affirmatively: fatigue as a kind of inverse expression of creative energy, which still carries with it the traces of our desire for novelty, invention, relation.   We will share and discuss conceptualisations and lived experiences of fatigue, the thresholding moments and political energies that open up between fatigue/desire, and build some movement propositions (e.g. dragging, slumping, surrendering) that can be shared and practised across digital and physical space.") "Gather-town + bUm, Kreuzberg" "Ally Bisshop" Nothing (Just <| Image 2 "https://lh3.googleusercontent.com/BY3bhvTAmKkJuQpJiqHaaiHx2St7N6MPb6J6XRn5y6eNuy5QDqcWwxWH22-keWDg-r03h_2vlWlqOZivqRpw0IYnDi7snyI6yLAfjQqN2khaTcvdT8fSybyqbJ_5w3oTkAgGxI2cCdGzZoxtQQ")
        , Event 24 Mar 22 "to enact a movement" (Just """“I invite you to begin to sink. To sink down into your chair or into the ground if you're lying down.
        And dissolve. Dissolve completely into a liquid as if you were not solid anymore, so that no part of you is being held. \u{200B}\u{200B}Feel yourself as the pile of bones wrapped in skin that you are.
        And now slowly, with attention, try to move.
        How can you move if you are separate pieces?
        What needs to happen to initiate and enact a response, a movement?” - Julia Metzger-Traber'
        In this moment, when the Russian invasion is currently devastating Ukraine and there is an ongoing global pandemic, there is a strong political act in coming together to move, to strengthen our bonds, expand our empathy and to enact a movement.
        In this lab we will explore the anarchic archive of ‘Moving across Thresholds’ scores and stories built over the last 30-events. We will then dissolve this body of work and reconstruct it in a way that generates thresholds of potential to help us think and move now, in these urgent times.
        > 'movement' noun: position change.
        > 'movement 'noun: a group of people with a particular set of aims or ideas""") "Gather-town + bUm, Kreuzberg" "Renae Shadler and Susanne Schmitt" Nothing (Just <| Image 2 "https://lh5.googleusercontent.com/yp-n-jN-3yQgZaDYlqotY1hlQ37AqPZPWV7WALfJPtj_Al-g5gsfkg5lb3BrIbUbjrdN71CZ6wQDCdjUDCmpDRkX9PW6BUf1EXHMQ1W0zz0-P0uxlU4VRnryWudH4bI9tcQrTQbHw79ZCfbNIg")
        , Event 7 Apr 2 "Moon Cancer Lightning" (Just """In an age where no one’s secrets are kept sacred, and we are all forced to be spies on each other’s realities; how do we connect with and express our innermost beings into a language recognisable by all?
        Driven by this question, guest facilitator - Tsuki, will guide us through a movement practice incorporating Ashtanga and Kundalini Yoga, Butoh and Contemporary Dance as well as Astrology. Tsuki sees every body as fundamentally transgender, with the power to transform between a multitude of expressions. In this event we will explore the thresholds of potential that lie between our perceived internal and external realities.
        Tsuki is a Performance Artist, Butoh and Ashtanga Yoga teacher based in Berlin. She leads Full Moon Performance Rituals and is focused on enabling spaces where the art, the community, and the survival can co-exist. Tsuki performs solo and collaborative performance projects influenced by her multidisciplinary training and has performed in Berlins most infamous night clubs and queer parties, including; Tresor, KitKat Club, Suicide Circus.""") "Gather-town + bUm, Kreuzberg" "Tsuki" Nothing (Just <| Image 2 "https://lh3.googleusercontent.com/6VR4GLZ2k7FOOI-AsQBKj6e2OZJDOchELAt7G3y7NS7zQnDmvHM70M-r-fOUDz2UpLU_n9vPuPxKqTLFF1Pr7s7QJYK9lD9wS0V5Nh9dYzUHX2IAUgSeX3sx83DYNKPe83V4D7Wiz6_koaAOFQ")
        , Event 21 Apr 22 "wailing is breathing out loud for others" (Just """Believing that we can only imagine futures of an otherwise if we acknowledge the grieves in this world, performance artist, researcher, and death doula - Siegmar Zacharias, will propose a listening practice where we can hold space for each other's grieving. This particular practice started with a lament her great grandmother sang as a wailing woman in Romania and works with solfeggio frequencies, the same frequency bees wings produce. They will be allies in re-generating while being in the quaking.
Siegmar Zacharias collaborates with humans and uncontrollable materials, such as smoke, slime, drool, the nervous system and death/dying/grief. Together with Steve Heather she is exploring binaural sound for deep listening and deep touch. She is indebted to cultures of listening with the whole body, merging somatic practices, performance practices, herbalism, social work and visceral thinking together.""") "Gather-town + bUm, Kreuzberg" "Siegmar Zacharias" Nothing (Just <| Image 2 "https://lh4.googleusercontent.com/KwejzH-Kkat6LUS1_jly3jxyYyq3J2LxQ4FJTA6Zk0SGRah7qvjZWeHGCI6xpoKHaBHkOP_cE5Rry0frssakrd0L_r8MgORyYYu4VSwVnl-qXX4s76CcZ53G3obn02u0IvDmJ8b8ol4mh5b6KA")
        ]
    , Series 5
        "QUEERING"
        [ Event 12 May 22 "web of nonhuman forces" (Just """What do tree pollen, the North American electrical grid, potato chips, and stem cells all have in common? To political theorist Jane Bennett, they are all essentially alive, pulsing and vibrating with a vital materiality that unifies all things - living and non-living, human and non-human.
Facilitated by choreographer/dancer Renae Shadler and anthropologist Susanne Schmitt, this event will focus on Bennett’s 2010 book VIBRANT MATTER where she proposes a philosophy that ties the universe together not through an omniscient god, but by the very material stuff that makes up every thing, substance, place, and person in the world. Together, we will read an excerpt of the text and explore movement propositions that shift the focus from the human experience of things to the things themselves, emphasizing the active participation of nonhuman forces present.""") "Gather-town + bUm, Kreuzberg" "Renae Shadler and Susanne Schmitt" Nothing (Just <| Image 2 "https://lh4.googleusercontent.com/VhOAuWZhdvGKJOh8n1zQjrl9Ct7ss_7Z7pYROGkU7D1lGXJLE9NJa1BmpOSZyonKS-gAJKrESPjsOzeBqylNTWqhfggGCjLCh1Z2UQGItgsmigvgRJzD2y2e3vm3XnB9MbFEANVVBX6x8XVP5g")
        , Event 26 May 22 "Slow Transitions" (Just """Slow Transitions is a gathering facilitated by Natal Igor Dobkin that investigates the temporality of slowness as a method for change. The gathering will explore ‘mythical’ linear movements and how they can become radical when done in slow motion, such as with gender transitions, aging and even the process of committing to a relationship.
Natal Igor Dobkin, based both in Berlin and Tel aviv, is a performance artist, teacher, and adjunct professor in the department of gender studies at Ben Gurion University. He/They teach a performance-art course at 'The Garage' for students facing mental health challenges and regularly facilitates independent workshops in Tel Aviv that deal with the performer’s identity in sexual contexts and the stage world.""") "Gather-town + bUm, Kreuzberg" "Natal Igor Dobkin" Nothing (Just <| Image 2 "https://lh3.googleusercontent.com/c3-fWmyLIwcgbX6FXCAO_9KGeufPxFxU_vWZSRnYuGHURU1fmrF_ov6F_uxmjzRJj7oHcL2VZX5BiG4PQmgIORAsvx3iu3Ud7euiG5PQ2_kytc1W6GUUXtbTHwVodHNCI30j5ZRzi0yFC5UWww")
        , Event 9 Jun 22 "t.b.a. (Queering #3)" Nothing "Gather-town + bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 23 Jun 22 "t.b.a. (Queering #4)" Nothing "Gather-town + bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 7 Jul 22 "t.b.a. (Queering #5)" Nothing "Gather-town + bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 21 Jul 22 "t.b.a. (Queering #6)" Nothing "Gather-town + bUm, Kreuzberg" "t.b.a." Nothing Nothing
        ]
    , Series 6
        "LOCALITY"
        [ Event 8 Sep 22 "Locality 1" Nothing "Gather-town + at bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 22 Sep 22 "Locality 2" Nothing "Gather-town + at bUm, Kreuzberg" "t.b.a." Nothing (Just <| Image 1 "https://lh5.googleusercontent.com/8ty1IW3kx04YMqKk-xnWt4XGZ5YgxIesJPvSMZvJrxdYmzTb4SqoJTj-BXTfkAUJ2VkNc0x7Boo3DnAK9N89zYWv2ketrr0YmUR-Bhd9n_ThR2kdu2Bmcd7Ct8CQiWQ8cpXEq02bxkuXXmMz1A")
        , Event 6 Oct 22 "Locality 3" Nothing "Gather-town + at bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 20 Oct 22 "Locality 4" Nothing "Gather-town + at bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 3 Nov 22 "Locality 5" Nothing "Gather-town + at bUm, Kreuzberg" "t.b.a." Nothing Nothing
        , Event 17 Nov 22 "Locality 6" Nothing "Gather-town + at bUm, Kreuzberg" "t.b.a." Nothing Nothing
        ]
    ]


presets : Time.Zone -> List ( String, Segment.BodyTemplate )
presets timezone =
    data
        |> List.concatMap
            .events
        |> List.concatMap
            (\event ->
                (article []
                    [ h2 []
                        [ Occurrence.view (Occurrence.Short timezone Minutes) (eventDate timezone event) ]
                    , p []
                        [ b [] [ text "Facilitated by " ]
                        , a [ href (Layout.sanitise event.facilitator) ] [ text event.facilitator ]
                        , Maybe.map (\c -> text (" (" ++ c ++ ")")) event.country |> Maybe.withDefault Ui.none
                        ]
                    ]
                    |> Segment.Content (Just event.title)
                    |> Tuple.pair event.title
                )
                    :: collageToIllustration event
            )


eventDate : Time.Zone -> Event -> Occurrence
eventDate timezone event =
    Occurrence.moment timezone event.month event.day (2000 + event.year) 20 0
        |> Occurrence.withDurationMinutes 90


presetInfos : List ( String, Segment.InfoTemplate )
presetInfos =
    data
        |> List.map
            (\series ->
                Byline 0 (span [ class "motto" ] [ text series.motto ])
                    |> Tuple.pair ("Series " ++ String.fromInt series.number)
            )


eventbriteLink : String
eventbriteLink =
    "https://www.eventbrite.de/e/moving-across-thresholds-tickets-255265715627"


structure : Time.Zone -> List Action
structure timezone =
    data
        |> List.map
            (\series ->
                Name ("Series " ++ String.fromInt series.number)
                    :: Modify (Segment.WithShape (Oriented Horizontal (ViewSegment.Columns 1)))
                    :: Go Down
                    :: (List.map
                            (\event ->
                                let
                                    addFab =
                                        Modify <| Segment.WithFab (Just <| Fab.Register { link = eventbriteLink, occurrence = eventDate timezone event })
                                in
                                [ Name (event.title ++ "-")
                                , Modify <| Segment.WithCaption { text = event.title, showsDate = True }
                                , addFab
                                , Go Down
                                , Name event.title
                                , Modify <| Segment.WithShape (Oriented Horizontal (ViewSegment.Columns 1))
                                , addFab
                                ]
                                    ++ (case event.collage of
                                            Nothing ->
                                                []

                                            Just c ->
                                                [ Go Right
                                                , Name (event.title ++ " (Collage)")
                                                , Modify <| Segment.WithShape (Oriented Horizontal (ViewSegment.Columns (collageColumns c)))
                                                , Go Left
                                                ]
                                       )
                                    ++ [ Go Up ]
                            )
                            series.events
                            |> List.intersperse [ Go Right ]
                            |> List.concat
                       )
                    ++ [ Go Left, Go Up ]
            )
        |> List.intersperse [ Go Right ]
        |> List.concat
