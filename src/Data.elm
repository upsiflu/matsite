module Data.elm exposing (initial)

import Snippets.Artist as Artist

import Accordion exposing (Accordion, Action(..))
import Accordion.Segment as Segment
import DateTime exposing (DateTime)
import DateTime.Calendar as Calendar
import Time
import Loop

initial : Accordion msg
initial =
    let
        artists : List Action
        artists =
            Artist.artists
                |> List.map
                    (\({ name, wide } as artist) ->
                        Name (name ++ "(photo)")
                            :: Modify Horizontal
                            :: Modify (WithBody (Artist.viewPhoto artist))
                            :: if wide then [ Modify AddColumn ] else []
                            ++ Go Right
                            :: Name name
                            :: Modify Horizontal
                            :: Modify (WithBody (Artist.viewPhoto artist))
                            :: Modify (AddClass "fg")
                            :: []
                    )
                    |> List.intersperse (go Right)

        register : Int -> Int -> Int -> Int -> Action
        register day month year durationDays = 
            let 
                start =
                    Calendar.fromRawParts { day = day, month = month, year = year }
                        |> Maybe.withDefault (Calendar.fromPosix 0)
                end =
                    Loop.for durationDays Calendar.incrementDay start
            in
            Modify (WithFab {link = "TODO eventbrite link", from = Calendar.toPosix start, until = Calendar.toPosix end} )
        
        appendSubtree =
            Go Down
                :: Name "Perform[d]ance"
                :: register 25 11 2022 2
                :: Go Right
                :: Name "Radialsystem" 
                :: register 23 4 2022 1
                :: Go Down
                :: Name "Info" 
                :: Modify Horizontal
                :: Go Right
                :: Name "Collage" 
                :: Modify (WithBody Festival.collage)
                :: Modify Horizontal 
                :: Go Right
                :: Name "Description" 
                :: Modify (WithBody Festival.description)
                :: Modify Horizontal
                :: Go Right
                :: Name "Video" 
                :: Modify (WithBody Festival.video)
                :: Modify Horizontal
                :: Modify Segment.AddColumn
                :: Go Right
                :: Name "Credits"
                :: Modify Horizontal 
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
                >> go Up -}
    in
    
        Name "Home" Intro.intro
        :: Modify AsBackground
        :: Go Right
        :: Name "Labs"
        :: Modify <| Segment.WithInfo <| Segment.Byline "Biweekly on Thursdays; 90mins"
        :: Go Right
        :: Name "Festivals"
        :: Modify <| Segment.withInfo <| Segment.Byline "Text line - Festivals!"
        :: appendSubtree
        ++ Go Right
        :: Name "Artists"
        :: Go Down
        :: doArtists
        ++ Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Up
        :: Generate Accordion.Toc
        |> go Right
        |> set Vertical "Traces" Segment.None
        |> go Right
        |> set Vertical "Videos" Segment.None
        |> go Right
        |> set Vertical "Library" anarchiveX
        |> go Right
        |> set Vertical "About" Segment.None
        |> go Right
        |> set Vertical "Newsletter" Segment.None
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> root
    Accordion.create
        [

        ]
