module Data exposing (initial)

import Accordion exposing (Accordion, Action(..))
import Accordion.Segment as Segment exposing (Action(..), Orientation(..), Shape(..))
import Accordion.Segment.Fab as Fab
import Calendar
import DateTime exposing (DateTime)
import Fold exposing (Direction(..), Foldr, Position, Role(..))
import Loop
import Occurrence exposing (Occurrence)
import Snippets.Artist as Artist
import Snippets.Festival as Festival
import Snippets.Intro as Intro
import Snippets.Lab as Lab
import Time exposing (Month(..))


initial : Accordion msg
initial =
    let
        artists : List Accordion.Action
        artists =
            Artist.artists
                |> List.map
                    (\({ name, wide } as artist) ->
                        Name (name ++ "(photo)")
                            :: Modify (WithShape (Oriented Horizontal))
                            :: Modify (WithTemplate (Artist.viewPhoto artist))
                            :: (if wide then
                                    [ Modify IncrementColumnCount
                                    ]

                                else
                                    []
                               )
                            ++ Go Right
                            :: Name name
                            :: Modify (WithShape (Oriented Horizontal))
                            :: Modify (WithHeading (Artist.viewHeading artist))
                            :: Modify (WithTemplate (Artist.view artist))
                            :: Modify (AddClass "fg")
                            :: []
                    )
                |> List.intersperse [ Go Right ]
                |> List.concat

        register : Occurrence -> Accordion.Action
        register occurrence =
            Fab.Register { link = "TODO eventbrite link", occurrence = occurrence }
                |> WithFab
                |> Modify

        register2 : Int -> Month -> Int -> Int -> Accordion.Action
        register2 day month year hour =
            Occurrence.moment Time.utc month day year hour 0
                |> Occurrence.withDurationMinutes 90
                |> register

        appendSubtree =
            Go Down
                :: Name "Perform[d]ance"
                :: register2 25 Nov 2022 2
                :: Go Right
                :: Name "Radialsystem"
                :: register2 23 Apr 2022 1
                :: Go Down
                :: Name "Info"
                :: Modify (WithShape (Oriented Horizontal))
                :: Go Right
                :: Name "Collage"
                :: Modify (WithTemplate Festival.collage)
                :: Modify (WithShape (Oriented Horizontal))
                :: Go Right
                :: Name "Description"
                :: Modify (WithTemplate Festival.description)
                :: Modify (WithShape (Oriented Horizontal))
                :: Go Right
                :: Name "Video"
                :: Modify (WithTemplate Festival.video)
                :: Modify (WithShape (Oriented Horizontal))
                :: Modify Segment.IncrementColumnCount
                :: Go Right
                :: Name "Credits"
                :: Modify (WithShape (Oriented Horizontal))
                :: Go Left
                :: Go Left
                :: Go Up
                :: Go Up
                :: []

        {- subtreeForLabs =
           go Down
               >> set2 Horizontal "Series 1" "2020"
               >> appendSubtree
               >> go Right
               >> set2 Horizontal "Series 2" "2021"
               >> appendSubtree
               >> go Right
               >> set2 Horizontal "Series 3" "2021"
               >> appendSubtree
               >> go Right
               >> set2 Horizontal "Series 4" "2022"
               >> appendSubtree
               >> go Right
               >> set2 Horizontal "Series 5" "2022"
               >> appendSubtree
               >> go Right
               >> set2 Horizontal "Series 6" "2022"
               >> appendSubtree
               >> go Left
               >> go Left
               >> go Up
        -}
    in
    Name "Home"
        :: Modify (WithTemplate Intro.intro)
        :: Modify (WithShape Segment.Background)
        :: Go Right
        :: Name "Labs"
        :: Modify
            (Segment.WithInfo <|
                Segment.Byline "Biweekly on Thursdays; 90mins"
            )
        :: Go Right
        :: Name "Festivals"
        :: Modify
            (Segment.WithInfo <|
                Segment.Byline "Text line - Festivals!"
            )
        :: appendSubtree
        ++ Go Right
        :: Name "Artists"
        :: Go Down
        :: artists
        ++ Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Up
        :: Generate Accordion.Toc
        :: Go Right
        :: Name "Traces"
        :: Go Right
        :: Name "Videos"
        :: Go Right
        :: Name "Library"
        :: Modify (WithTemplate Accordion.anarchiveX)
        :: Go Right
        :: Name "About"
        :: Go Right
        :: Name "Newsletter"
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Up
        :: []
        |> Accordion.create
