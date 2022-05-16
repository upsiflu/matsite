module Data exposing (addTemplates, initial, initialActions)

import Accordion exposing (Accordion, Action(..))
import Accordion.Segment as Segment exposing (Action(..), Orientation(..), Shape(..))
import Accordion.Segment.Fab as Fab
import Accordion.Segment.ViewMode as ViewSegment
import Dict
import Fold exposing (Direction(..), Role(..))
import Layout
import Occurrence exposing (Occurrence)
import Snippets.Anarchive as Anarchive
import Snippets.Artist as Artist
import Snippets.Festival as Festival
import Snippets.Intro as Intro
import Time exposing (Month(..))


addTemplates : Segment.Templates -> Segment.Templates
addTemplates =
    let
        presetBody key value t =
            { t | body = Dict.insert key ( True, value ) t.body }

        presetInfo key value t =
            { t | info = Dict.insert key ( True, value ) t.info }

        addArtistTemplates t =
            Artist.artists
                |> List.map
                    (\artist ->
                        presetBody (artist.name ++ "(photo)") (Artist.viewPhoto artist)
                            >> presetBody artist.name (Artist.view artist)
                    )
                |> List.foldl (<|) t

        addOtherTemplates =
            presetBody "Collage" Festival.collage
                >> presetBody "Video" Festival.video
                >> presetBody "Description" Festival.description
                >> presetBody "Home" Intro.intro
                >> presetBody "Library" Anarchive.anarchive
                >> presetInfo "Artists" Segment.Toc
                >> presetInfo "Labs" (Segment.Byline 1 (Layout.byline "Biweekly on Thursdays; 90mins"))
                >> presetInfo "Festivals" (Segment.Byline 1 (Layout.byline "Festivals Byline"))
    in
    addArtistTemplates >> addOtherTemplates


initial : Accordion msg
initial =
    initialActions
        |> Accordion.create
        |> Accordion.mapTemplates addTemplates


initialActions : List Accordion.Action
initialActions =
    let
        artists : List Accordion.Action
        artists =
            Artist.artists
                |> List.map
                    (\({ name, wide } as artist) ->
                        [ Name (name ++ "(photo)")
                        , Modify
                            (WithShape
                                (Oriented Horizontal
                                    (if wide then
                                        ViewSegment.Columns 2

                                     else
                                        ViewSegment.Columns 1
                                    )
                                )
                            )
                        , Go Right
                        , Name name
                        , Modify (WithShape (Oriented Horizontal (ViewSegment.Columns 1)))
                        , Modify (WithClasses [ "fg" ])
                        ]
                    )
                |> List.intersperse [ Go Right ]
                |> List.concat

        register : Occurrence -> Accordion.Action
        register occurrence =
            Fab.Register { link = "TODO eventbrite link", occurrence = occurrence }
                |> Just
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
                :: Modify (WithShape (Oriented Horizontal (ViewSegment.Columns 1)))
                :: Go Right
                :: Name "Collage"
                :: Modify (WithShape (Oriented Horizontal (ViewSegment.Columns 1)))
                :: Go Right
                :: Name "Description"
                :: Modify (WithShape (Oriented Horizontal (ViewSegment.Columns 1)))
                :: Go Right
                :: Name "Video"
                :: Modify (WithShape (Oriented Horizontal (ViewSegment.Columns 2)))
                :: Go Right
                :: Name "Credits"
                :: Modify (WithShape (Oriented Horizontal (ViewSegment.Columns 1)))
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
        :: Modify (WithShape Segment.Background)
        :: Go Right
        :: Name "Labs"
        :: Go Right
        :: Name "Festivals"
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
        :: Go Right
        :: Name "Traces"
        :: Go Right
        :: Name "Videos"
        :: Go Right
        :: Name "Library"
        :: Go Right
        :: Name "About"
        :: Go Right
        :: Name "Newsletter"
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: []
