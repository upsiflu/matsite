module Snippets.Series exposing (..)

import Accordion exposing (Action(..))
import Accordion.Segment as Segment exposing (InfoTemplate(..), Orientation(..), Shape(..))
import Accordion.Segment.Fab as Fab exposing (Fab)
import Accordion.Segment.ViewMode as ViewSegment
import Fold exposing (Direction(..))
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layout
import List.Extra as List
import Occurrence exposing (Occurrence, Precision(..))
import Snippets.Festival exposing (Festival)
import Snippets.Lab exposing (Lab)
import Time exposing (Month(..))


type alias Event =
    { day : Int, month : Month, year : Int, title : String, location : String, facilitator : String, country : Maybe String }


type alias Series =
    { number : Int, motto : String, events : List Event }


data =
    [ Series 1
        "LOWERING THE THRESHOLD"
        [ Event 8 Oct 20 "Erin Manning conversation" "bUM - Live event & Zoom" "Erin Manning" (Just "CA")
        , Event 17 Oct 20 "backwards walking brunch" "bUM - Live event" "Renae Shadler" Nothing
        , Event 23 Oct 20 "with infrasound / with me" "bUM - Live event" "Renae Shadler" Nothing
        , Event 6 Nov 20 "digital pond" "Zoom" "Renae Shadler" Nothing
        , Event 7 Nov 20 "middling brunch" "Zoom" "Renae Shadler" Nothing
        , Event 20 Nov 20 "internet imaginaries" "Zoom" "Renae Shadler" Nothing
        , Event 4 Dec 20 "dances of the mouth" "Zoom" "Mmakgosi Kgabi" (Just "BW/DE")
        , Event 12 Dec 20 "Access and Excess" "Zoom" "Balam Kenter" (Just "TR")
        , Event 18 Dec 20 "dances of care" "Zoom" "Sindri Runudde" (Just "SE")
        ]
    , Series 2
        "ECOLOGY"
        [ Event 15 Jan 21 "parallel worlds" "Zoom" "Renae Shadler with Roland Walter" (Just "DE")
        , Event 23 Jan 21 "coming together and in relay" "Gather-town" "Renae Shadler" Nothing
        , Event 29 Jan 21 "drifting" "Gather-town" "Renae Shadler" Nothing
        , Event 12 Feb 21 "exploring the virtual" "Gather-town" "Renae Shadler" Nothing
        , Event 20 Feb 21 "Tender Hotel edition" "Zoom ** part of Tender Hotel festival" "Renae Shadler" Nothing
        , Event 26 Feb 21 "creating a Verbal Climate" "Gather-town" "Rivca Rubin" (Just "UK")
        , Event 12 Mar 21 "Botanizing the virtual" "Gather-town" "Susanne Schmitt" (Just "DE")
        , Event 20 Mar 21 "Wandering the City" "Gather-town & city walk" "Anna Mayberry" (Just "FR/DE")
        , Event 26 Mar 21 "the Falling Sky" "Gather-town" "Renae Shadler" Nothing
        ]
    , Series 3
        "INTERRELATION"
        [ Event 9 Apr 21 "layers of cells/earth/a.../?" "Gather-town" "Cinzia Schincariol" (Just "IT")
        , Event 17 Apr 21 "building habitats and interconnections" "Gather-town" "Renae Shadler" Nothing
        , Event 23 Apr 21 "stream of emergent patters" "Gather-town" "Renae Shadler" Nothing
        , Event 7 May 21 "Nonhuman Sensations in Technoplanetary Layers" "Gather-town" "Renae Shadler" Nothing
        , Event 15 May 21 "Tentacular Thinking" "Gather-town" "Renae Shadler" Nothing
        , Event 21 May 21 "Facing the Impossible" "Gather-town" "Maikon K" (Just "BR")
        , Event 11 Jun 21 "staying with the muddle" "Gather-town" "Julia Grillmayr" (Just "AT")
        , Event 19 Jun 21 "imperfect intimacy" "Gather-town" "Renae Shadler" Nothing
        , Event 25 Jun 21 "social dreaming" "Gather-town" "Renae Shadler" Nothing
        ]
    , Series 4 "EMERGENCE" []
    , Series 5 "QUEERING" []
    , Series 6 "LOCALITY" []
    ]


presets : Time.Zone -> List ( String, Segment.BodyTemplate )
presets timezone =
    data
        |> List.map
            .events
        |> List.concat
        |> List.map
            (\event ->
                article []
                    [ h2 []
                        [ Occurrence.view (Occurrence.Short timezone Minutes) (eventDate timezone event) ]
                    , p [] [ b [] [ text "Facilitated by " ], a [ href (Layout.sanitise event.facilitator) ] [ text event.facilitator ] ]
                    , p [] (Maybe.map (text >> List.singleton) event.country |> Maybe.withDefault [])
                    ]
                    |> Segment.Content (Just event.title)
                    |> Tuple.pair event.title
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
                                [ Name event.title
                                , Modify <| Segment.WithShape (Oriented Horizontal (ViewSegment.Columns 1))
                                , Modify <| Segment.WithFab (Just <| Fab.Register { link = "TODO eventbrite link", occurrence = eventDate timezone event })
                                ]
                            )
                            series.events
                            |> List.intersperse [ Go Left ]
                            |> List.concat
                       )
                    ++ [ Go Up ]
            )
        |> List.intersperse [ Go Right ]
        |> List.concat
