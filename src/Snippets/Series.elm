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
    case event.collage of
        Just (Image _ str) ->
            [ img [ src str, title event.title ] []
                |> Segment.Illustration
                |> Tuple.pair (event.title ++ " (Collage)")
            ]

        Just (Vimeo _ int) ->
            [ Video.vimeoVideo int
                |> Segment.Illustration
                |> Tuple.pair (event.title ++ " (Collage)")
            ]

        Nothing ->
            []


type alias Event =
    { day : Int, month : Month, year : Int, title : String, location : String, facilitator : String, country : Maybe String, collage : Maybe Collage }


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
        [ Event 8 Oct 20 "Erin Manning conversation" "bUM - Live event & Zoom" "Erin Manning" (Just "CA") Nothing
        , Event 17 Oct 20 "backwards walking brunch" "bUM - Live event" "Renae Shadler" Nothing Nothing
        , Event 23 Oct 20 "with infrasound / with me" "bUM - Live event" "Renae Shadler" Nothing Nothing
        , Event 6 Nov 20 "digital pond" "Zoom" "Renae Shadler" Nothing (Just <| Image 2 "https://scontent-ber1-1.xx.fbcdn.net/v/t1.6435-9/122970036_1757358807761830_1602188783427953404_n.jpg?_nc_cat=111&ccb=1-7&_nc_sid=340051&_nc_ohc=4Crh4wjrcicAX8Y25xG&_nc_ht=scontent-ber1-1.xx&oh=00_AT-K2kw0z_XdATAiftLDNCs4trJyJjh6rSkh2wikFyEzFA&oe=62AE7945")
        , Event 7 Nov 20 "middling brunch" "Zoom" "Renae Shadler" Nothing Nothing
        , Event 20 Nov 20 "internet imaginaries" "Zoom" "Renae Shadler" Nothing Nothing
        , Event 4 Dec 20 "dances of the mouth" "Zoom" "Mmakgosi Kgabi" (Just "BW/DE") Nothing
        , Event 12 Dec 20 "Access and Excess" "Zoom" "Balam Kenter" (Just "TR") Nothing
        , Event 18 Dec 20 "dances of care" "Zoom" "Sindri Runudde" (Just "SE") (Just <| Vimeo 3 517426338)
        ]
    , Series 2
        "ECOLOGY"
        [ Event 15 Jan 21 "parallel worlds" "Zoom" "Renae Shadler with Roland Walter" (Just "DE") Nothing
        , Event 23 Jan 21 "coming together and in relay" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 29 Jan 21 "drifting" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 12 Feb 21 "exploring the virtual" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 20 Feb 21 "Tender Hotel edition" "Zoom ** part of Tender Hotel festival" "Renae Shadler" Nothing Nothing
        , Event 26 Feb 21 "creating a Verbal Climate" "Gather-town" "Rivca Rubin" (Just "UK") Nothing
        , Event 12 Mar 21 "Botanizing the virtual" "Gather-town" "Susanne Schmitt" (Just "DE") Nothing
        , Event 20 Mar 21 "Wandering the City" "Gather-town & city walk" "Anna Mayberry" (Just "FR/DE") Nothing
        , Event 26 Mar 21 "the Falling Sky" "Gather-town" "Renae Shadler" Nothing (Just <| Image 2 "https://scontent-ber1-1.xx.fbcdn.net/v/t1.6435-9/163478184_1880023925495317_6224846316291674014_n.jpg?_nc_cat=102&ccb=1-7&_nc_sid=340051&_nc_ohc=VCO6q1J7KKUAX-wWxoi&_nc_ht=scontent-ber1-1.xx&oh=00_AT-6dwvCG5Z_AL9bNS8hKHPHNPs9rL1r0k8SZ6Ev-TdZjA&oe=62ADB6C1")
        ]
    , Series 3
        "INTERRELATION"
        [ Event 9 Apr 21 "layers of cells/earth/a.../?" "Gather-town" "Cinzia Schincariol" (Just "IT") Nothing
        , Event 17 Apr 21 "building habitats and interconnections" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 23 Apr 21 "stream of emergent patters" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 7 May 21 "Nonhuman Sensations in Technoplanetary Layers" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 15 May 21 "Tentacular Thinking" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 21 May 21 "Facing the Impossible" "Gather-town" "Maikon K" (Just "BR") Nothing
        , Event 11 Jun 21 "staying with the muddle" "Gather-town" "Julia Grillmayr" (Just "AT") Nothing
        , Event 19 Jun 21 "imperfect intimacy" "Gather-town" "Renae Shadler" Nothing (Just <| Image 3 "https://scontent-ber1-1.xx.fbcdn.net/v/t1.6435-9/200459577_1945726685591707_6985922393600567487_n.jpg?stp=cp0_dst-jpg_e15_fr_q65&_nc_cat=104&ccb=1-7&_nc_sid=ed5ff1&efg=eyJpIjoidCJ9&_nc_ohc=AbQTMRbkl_QAX8kMpno&_nc_ht=scontent-ber1-1.xx&oh=00_AT_Ue-YS83kY3ry3HZMCZALAgLPaZlCQFMEctMydgjTi6A&oe=62AEC38B")
        , Event 25 Jun 21 "social dreaming" "Gather-town" "Renae Shadler" Nothing Nothing
        ]
    , Series 4
        "EMERGENCE"
        [ Event 17 Apr 22 "Placeholder - still needs to be filled with data" "Gather-town" "Renae Shadler" Nothing (Just <| Image 1 "https://lh3.googleusercontent.com/z1V3YjIBvBWedW_QyUA6jvgTGWv50_OxRF00nsOQEgLIVvvFdKh_oJ-_5pMV6WBBgY8hOFuKZ68iK5Mn-41xNoSl5anfCs00h7VQ6zIYB9eOGulNk_zmhe68lJXTPxtNKhtHL2gITa7E-iGhCA") ]
    , Series 5
        "QUEERING"
        [ Event 17 Apr 22 "Placeholder - still needs to be filled with data" "Gather-town" "Renae Shadler" Nothing (Just <| Image 1 "https://lh5.googleusercontent.com/NQzCaVWSrkYR7CHK6Zg8A5YsbB8pw6K5DMRa5lm1RcufpMUqUCYEzgiGXB2G_4LeK7N76R29ebuF1BqE5MugQWbKiyAAUUCNgJKiXXdBOPgYatebcyMqPkS0sYtjjId6E0OI47n-jtH78z3bxA") ]
    , Series 6
        "LOCALITY"
        [ Event 17 Apr 22 "Z / Test for past Date - the register button should be hidden in past dates" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 17 Jul 22 "A / Test for future Dates" "Gather-town" "Renae Shadler" Nothing (Just <| Image 1 "https://lh5.googleusercontent.com/8ty1IW3kx04YMqKk-xnWt4XGZ5YgxIesJPvSMZvJrxdYmzTb4SqoJTj-BXTfkAUJ2VkNc0x7Boo3DnAK9N89zYWv2ketrr0YmUR-Bhd9n_ThR2kdu2Bmcd7Ct8CQiWQ8cpXEq02bxkuXXmMz1A")
        , Event 21 Jul 22 "B / Test for future Dates" "Gather-town" "Renae Shadler" Nothing Nothing
        , Event 28 Jul 22 "C / You should see the Register Button in future events, and for the whole series, the next event should be proposed." "Gather-town" "Renae Shadler" Nothing Nothing
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
                    , p [] [ b [] [ text "Facilitated by " ], a [ href (Layout.sanitise event.facilitator) ] [ text event.facilitator ] ]
                    , p [] (Maybe.map (text >> List.singleton) event.country |> Maybe.withDefault [])
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
                                [ Name event.title
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
                                                , addFab
                                                ]
                                       )
                            )
                            series.events
                            |> List.intersperse [ Go Right ]
                            |> List.concat
                       )
                    ++ [ Go Left, Go Up ]
            )
        |> List.intersperse [ Go Right ]
        |> List.concat
