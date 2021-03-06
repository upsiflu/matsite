module Data exposing (addTemplates, initial, initialActions, initialIntents, initialTemplates)

import Accordion exposing (Accordion, Action(..))
import Article as Article exposing (Action(..), Orientation(..), Shape(..))
import Article.Fab as Fab
import Dict
import Directory exposing (Directory)
import Fold exposing (Direction(..), Role(..))
import Layout
import List.Extra as List
import Occurrence exposing (Occurrence)
import Snippets.About as About
import Snippets.Anarchive as Anarchive
import Snippets.Artist as Artist
import Snippets.Festival as Festival
import Snippets.Intro as Intro
import Snippets.Series as Series
import Snippets.Traces as Traces
import Snippets.Video as Video
import String.Extra as String
import Time exposing (Month(..))


addTemplates : Time.Zone -> Article.Templates -> Article.Templates
addTemplates zone =
    let
        presetBody key value t =
            { t | body = Dict.insert (Layout.sanitise key) ( True, value ) t.body }

        presetInfo key value t =
            { t | info = Dict.insert (Layout.sanitise key) ( True, value ) t.info }

        addArtistTemplates t =
            Artist.artists
                |> List.map
                    (\artist ->
                        let
                            uid =
                                Layout.sanitise artist.name
                        in
                        presetBody (uid ++ "(photo)") (Artist.viewPhoto artist)
                            >> presetBody uid (Artist.view artist)
                    )
                |> List.foldl (<|) t

        addLabsTemplates t =
            [ Series.presets zone
                |> List.map
                    (\( key, value ) ->
                        presetBody key value
                    )
            , Series.presetInfos
                |> List.map
                    (\( key, value ) ->
                        presetInfo key value
                    )
            ]
                |> List.concat
                |> List.foldl (<|) t

        addOtherTemplates =
            presetBody "Collage" Festival.collage
                >> presetBody "Video1" Festival.video1
                >> presetBody "Video2" Festival.video2
                >> presetBody "Description" Festival.description
                >> presetBody "Credits" Festival.radialsystemCredits
                >> presetBody "Home" Intro.intro
                >> presetBody "Incipit" Anarchive.incipit
                >> presetBody "Archive" Anarchive.anarchive
                >> presetBody "About MaT" About.mat
                >> presetBody "Team" About.team
                >> presetBody "Trailers" Video.trailers
                >> presetBody "Video Channel" Video.videochannel
                >> presetBody "Collective Docs" Traces.traces
                >> presetBody "Contact" About.contact
                >> presetInfo "Artists" Article.Toc
                >> presetInfo "Labs" (Article.Byline 1 (Layout.byline "Biweekly on Thursdays; 90mins"))
                >> presetBody "Festival (Illustration)" Festival.genericIllustration
                >> presetBody "Tidal Shifts" Festival.tidalShifts
    in
    addArtistTemplates >> addLabsTemplates >> addOtherTemplates


initial : Time.Zone -> Accordion
initial zone =
    Accordion.create (initialTemplates zone) (initialIntents zone)


initialIntents : Time.Zone -> Accordion.History
initialIntents =
    initialActions
        >> List.indexedMap (\i a -> { intentId = { sessionId = "Initialization", ordinal = i }, location = Nothing, action = a })


{-| These should only be used while editing
-}
initialTemplates : Time.Zone -> Article.Templates
initialTemplates zone =
    addTemplates zone Article.initialTemplates


subscribeLink : String
subscribeLink =
    "mailto:movingAcrossThresholds@gmail.com?subject=Subscription%20to%20the%20Moving%20across%Thresholds%20Newsletter&body=Dear%20Moving%20across%Thresholds%20Team%21%20Please%20add%20my%address%20to%20the%20list%3A"


initialActions : Time.Zone -> List Accordion.Action
initialActions timezone =
    let
        artists : List Accordion.Action
        artists =
            Artist.artists
                |> List.sortBy (\{ name } -> String.words name |> List.last |> Maybe.withDefault name)
                |> List.map
                    (\{ name, wide } ->
                        [ Name (name ++ "(photo)")
                        , Modify
                            (WithShape
                                (Oriented Horizontal
                                    (if wide then
                                        Article.Columns 2

                                     else
                                        Article.Columns 1
                                    )
                                )
                            )
                        , Go Right
                        , Name name
                        , Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
                        , Modify (WithClasses [ "fg" ])
                        ]
                    )
                |> List.intersperse [ Go Right ]
                |> List.concat

        register : Occurrence -> Accordion.Action
        register occurrence =
            Fab.Register { link = Series.eventbriteLink, occurrence = occurrence }
                |> Just
                |> WithFab
                |> Modify

        registerTwoDays : Int -> Month -> Int -> Int -> Accordion.Action
        registerTwoDays day month year hour =
            Occurrence.moment Time.utc month day year hour 0
                |> Occurrence.withDurationDays 1
                |> register

        appendSubtree =
            Go Down
                :: Name "Perform[d]ance"
                :: registerTwoDays 26 Nov 2022 2
                :: Modify (WithCaption { text = "Perform[d]ance Stralsund", showsDate = True })
                :: Go Down
                :: Name "Tidal Shifts"
                :: Go Down
                :: Name "Festival (Illustration)"
                :: Go Up
                :: Go Up
                :: Go Right
                :: Name "Radialsystem"
                :: Modify (WithCaption { text = "Radialsystem Berlin", showsDate = True })
                :: registerTwoDays 23 Apr 2022 1
                :: Go Down
                :: Name "Video2"
                :: Modify (WithShape (Oriented Horizontal (Article.Columns 2)))
                :: Go Right
                :: Name "Collage"
                :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
                :: Go Right
                :: Name "Description"
                :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
                :: Go Right
                :: Name "Video1"
                :: Modify (WithShape (Oriented Horizontal (Article.Columns 2)))
                :: Go Right
                :: Name "Credits"
                :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
                :: Go Left
                :: Go Left
                :: Go Up
                :: Go Up
                :: []
    in
    Name "Home"
        :: Modify (WithShape Article.Background)
        :: Modify (WithFab (Just <| Fab.Subscribe { link = subscribeLink }))
        :: Go Right
        :: Name "Labs"
        :: Go Down
        :: Series.structure timezone
        ++ Go Left
        :: Go Up
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
        :: Go Up
        :: Go Right
        :: Name "Traces"
        :: Go Down
        :: Name "Collective Docs"
        :: Modify (WithShape (Oriented Vertical Article.Screen))
        :: Go Up
        :: Go Right
        :: Name "Videos"
        :: Go Down
        :: Name "Video Channel"
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
        :: Go Right
        :: Name "Trailers"
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 3)))
        :: Go Left
        :: Go Up
        :: Go Right
        :: Name "Library"
        :: Go Down
        :: Name "Incipit"
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
        :: Go Right
        :: Name "Archive"
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 2)))
        :: Go Up
        :: Go Right
        :: Name "About"
        :: Modify (WithFab (Just <| Fab.Subscribe { link = subscribeLink }))
        :: Go Down
        :: Name "Contact"
        :: Modify (WithFab (Just <| Fab.Subscribe { link = subscribeLink }))
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
        :: Go Right
        :: Name "About MaT"
        :: Modify (WithFab (Just <| Fab.Subscribe { link = subscribeLink }))
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
        :: Go Right
        :: Name "Team"
        :: Modify (WithFab (Just <| Fab.Subscribe { link = subscribeLink }))
        :: Modify (WithShape (Oriented Horizontal (Article.Columns 1)))
        :: Go Left
        :: Go Up
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: Go Left
        :: []
