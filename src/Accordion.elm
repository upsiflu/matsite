module Accordion exposing
    ( Accordion
    , site
    , flip
    , find, root
    , location, focus
    , view
    , anarchiveX, vimeoX
    )

{-|


## To Do

  - [ ] Each `view` produces all Segments, in order of the alphabet
  - [ ] `flip` simply goes up one level

---

@docs Accordion
@docs site


# Modify

@docs flip


# Navigate

@docs find, root


# Deconstruct

Get textual representations fo use in Url and animation

@docs location, focus


# View

@docs Remainder, view

---

@docs anarchiveX, vimeoX

-}

import Accordion.Attributable as Cls exposing (Att)
import Accordion.Segment as Segment exposing (Orientation(..), Segment)
import Accordion.Segment.ViewMode as ViewSegment exposing (Offset, Region(..), ViewMode, Width(..))
import Css exposing (..)
import Fold exposing (Direction(..), Foldr, Position, Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Layout
import Snippets.Artist as Artist
import Snippets.Festival as Festival
import Snippets.Intro as Intro
import Snippets.Lab as Lab
import String exposing (left)
import Url exposing (Url)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Mixed as MixedZipper exposing (MixedZipper)
import Zipper.Tree as Tree exposing (Edge(..), EdgeOperation(..), Tree, Walk(..))


{-| -}
type Accordion msg
    = Accordion { tree : Tree (Segment msg), collapsed : Bool }


{-| -}
singleton : Tree (Segment msg) -> Accordion msg
singleton tree =
    Accordion { tree = tree, collapsed = False }


{-| -}
flip : Accordion msg -> Accordion msg
flip (Accordion config) =
    Accordion { config | collapsed = not config.collapsed }


{-| -}
root : Accordion msg -> Accordion msg
root (Accordion config) =
    Accordion { config | tree = Tree.root config.tree }


{-| -}
reset : Accordion msg -> Accordion msg
reset (Accordion config) =
    Accordion { config | collapsed = False }


{-| -}
location : Accordion msg -> String
location (Accordion config) =
    if Tree.isRoot config.tree then
        ""

    else
        config.tree |> Tree.up |> Tree.focus |> .id


{-| -}
focus : Accordion msg -> String
focus (Accordion config) =
    if Tree.isRoot config.tree then
        ""

    else
        config.tree |> Tree.focus |> .id


{-| The Url encodes the parent of the focus!
-}
find : Url -> Accordion msg -> Accordion msg
find { fragment } =
    let
        goToId =
            \str -> .id >> (==) str |> Find |> Tree.go
    in
    case fragment of
        Just parent ->
            mapTree (goToId parent >> Tree.go (Walk Down (Fail identity)))
                >> reset

        Nothing ->
            mapTree Tree.root


{-| -}
mapTree : (Tree (Segment msg) -> Tree (Segment msg)) -> Accordion msg -> Accordion msg
mapTree fu (Accordion config) =
    Accordion { config | tree = fu config.tree }


go : Direction -> Accordion msg -> Accordion msg
go direction =
    Branch.singleton Segment.empty |> Insert |> Walk direction |> Tree.go |> mapTree


set : Orientation -> String -> Maybe (Html msg) -> Accordion msg -> Accordion msg
set orientation caption body =
    Segment.singleton caption
        |> Segment.withOrientation orientation
        |> (case body of
                Nothing ->
                    identity

                Just b ->
                    Segment.withBody b
           )
        |> setSegment


setSegment : Segment msg -> Accordion msg -> Accordion msg
setSegment segment ((Accordion { tree }) as accordion) =
    let
        id =
            segment.id

        autoSuffix : Int -> String
        autoSuffix int =
            let
                testId =
                    id ++ "(" ++ String.fromInt int ++ ")"
            in
            if Tree.any (.id >> (==) testId) tree then
                autoSuffix (int + 1)

            else
                testId

        uniqueId =
            if Tree.any (.id >> (==) id) tree then
                autoSuffix 0

            else
                id
    in
    mapTree (Tree.mapFocus (\_ -> { segment | id = uniqueId })) accordion


mapSegment : (Segment msg -> Segment msg) -> Accordion msg -> Accordion msg
mapSegment =
    Tree.mapFocus >> mapTree


{-| -}
site : Accordion msg
site =
    let
        verticalSegment x =
            Segment.singleton x
                |> Segment.withOrientation Vertical
                |> Branch.singleton

        anarchive =
            verticalSegment "Anarchive"

        artists =
            Artist.artists
                |> List.map
                    (\{ name, bio, photo } ->
                        set Horizontal (name ++ "(photo)") (Just (Html.img [ class "artist", src photo ] []))
                            >> go Right
                            >> set Horizontal
                                name
                                (Just
                                    (Html.div [ class "artist richtext" ]
                                        [ Html.h2 [] [ Html.text name ]
                                        , bio
                                        ]
                                    )
                                )
                            >> mapSegment (Segment.withAdditionalAttributes [ class "fg" ])
                            >> go Right
                    )

        doArtists =
            List.foldl (\fu0 fu1 -> fu0 >> fu1) identity artists

        series =
            String.fromInt >> (++) "Series "

        set2 : Orientation -> String -> String -> Accordion msg -> Accordion msg
        set2 orientation cap1 cap2 =
            Segment.singleton cap1
                |> Segment.withOrientation orientation
                |> Segment.withAdditionalCaption cap2
                |> setSegment

        appendSubtree =
            go Down
                >> set2 Vertical "Perform[d]ance" "November 25-27"
                >> go Right
                >> set2 Vertical "Radialsystem" "April 23 + 24"
                >> go Down
                >> set Horizontal "Info" Nothing
                >> go Right
                >> set Horizontal "Collage" (Just Festival.collage)
                >> go Right
                >> set Horizontal "Description" (Just Festival.description)
                >> go Right
                >> set Horizontal "Video" (Just Festival.video)
                >> mapSegment Segment.increaseColumnCount
                >> go Right
                >> set Horizontal "Credits" Nothing
                >> go Left
                >> go Left
                >> go Up
                >> go Up

        subtreeForLabs =
            go Down
                >> set2 Horizontal "Series 1" "2020"
                >> go Right
                >> set2 Horizontal "Series 2" "2021"
                >> go Right
                >> set2 Horizontal "Series 3" "2021"
                >> go Right
                >> set2 Horizontal "Series 4" "2022"
                >> go Right
                >> set2 Horizontal "Series 5" "2022"
                >> go Right
                >> set2 Horizontal "Series 6" "2022"
                >> go Left
                >> go Left
                >> go Up
    in
    Tree.singleton Segment.empty
        |> singleton
        |> set Vertical "Home" (Just Intro.intro)
        |> go Right
        |> set Vertical "Labs" Nothing
        |> mapSegment (Segment.withInfo <| Html.text "Text line - biweekly 90mins")
        |> subtreeForLabs
        |> go Right
        |> set Vertical "Festivals" Nothing
        |> mapSegment (Segment.withInfo <| Html.text "Text line - biweekly 90mins")
        |> appendSubtree
        |> go Right
        |> set Vertical "Artists" Nothing
        |> mapSegment (Segment.withInfo <| Html.text "Text line - biweekly 90mins")
        |> go Down
        |> doArtists
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Up
        |> go Right
        |> set Vertical "Traces" Nothing
        |> go Right
        |> set Vertical "Videos" Nothing
        |> go Right
        |> set Vertical "Library" (Just anarchiveX)
        |> go Right
        |> set Vertical "About" Nothing
        |> go Right
        |> set Vertical "Newsletter" Nothing
        |> go Left
        |> go Down
        |> set Horizontal (series 6) Nothing
        |> go Left
        |> set Horizontal (series 5) Nothing
        |> go Left
        |> set Horizontal (series 4) Nothing
        |> go Left
        |> set Horizontal (series 3) Nothing
        |> go Right
        |> go Right
        |> go Down
        |> go Right
        |> set Horizontal "Festival" Nothing
        -- (Just <| )
        |> mapSegment (Segment.withInfo <| Html.text "Text line - festival popups, physical participation")
        |> appendSubtree
        |> go Right
        |> set Horizontal "Artist" Nothing
        |> go Down
        |> doArtists
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Up
        |> go Up
        |> go Up
        |> go Left
        |> go Left
        |> go Left


{-| -}
anarchiveX : Html msg
anarchiveX =
    Html.div [ class "anArchive opening" ]
        [ Html.iframe
            [ attribute "width" "100%"
            , css [ position absolute, Css.height (pct 100), border (px 0) ]
            , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
            , title "Moving Across Thresholds - AnArchive"
            ]
            []
        ]


{-| -}
vimeoX : Html msg
vimeoX =
    Html.iframe
        [ attribute "width" "100%"
        , css [ position absolute, Css.height (vh 100), border (px 0) ]
        , attribute "loading" "lazy"
        , src "https://player.vimeo.com/video/643915247?title=0&amp;byline=0&amp;portrait=0&amp;badge=0&amp;quality=1080p&amp;dnt=1"
        , attribute "frameborder" "0"
        , attribute "allowfullscreen" "allowfullscreen"
        , attribute "data-rocket-lazyload" "fitvidscompatible"
        , attribute "data-lazy-src" "https://player.vimeo.com/video/643915247?title=0&amp;byline=0&amp;portrait=0&amp;badge=0&amp;quality=1080p&amp;dnt=1"
        , attribute "data-ll-status" "loaded"
        , class "entered lazyloaded"
        ]
        []



---- View ----


type Renderable msg
    = Item ( String, Html msg )
    | Var ( String, Int )
    | Class String


{-| -}
view : Accordion msg -> Html msg
view (Accordion config) =
    let
        classes : List (Html.Attribute msg)
        classes =
            List.map class
                [ Segment.orientationToString (.orientation (Tree.focus config.tree)) ]

        createRegions : C msg -> List ( Region, List (A msg) )
        createRegions { up, left, x, here, nest, y, right, down } =
            [ ( North, up )
            , ( West, left )
            , ( NearWest, x )
            , ( Center, [ here ] )
            , ( Peek, nest )
            , ( NearEast, y )
            , ( East, right )
            , ( South, down )
            ]

        renderRegion : ( Region, List (A msg) ) -> List (Renderable msg)
        renderRegion ( region, list ) =
            List.foldl
                (\( position, segment ) ( offset, newList ) ->
                    ( ViewSegment.addWidth segment.width offset
                    , Segment.view { position = position, region = region, offset = offset } segment :: newList
                    )
                )
                ( ViewSegment.zeroOffset, [] )
                list
                |> (\( totalOffset, renderedSegments ) ->
                        ViewSegment.offsetToCssVariables totalOffset
                            |> List.map (Tuple.mapFirst ((++) (ViewSegment.regionToString region ++ "-")) >> Var)
                            |> (++) (List.map Item renderedSegments)
                   )

        renderAccordion : List (Renderable msg) -> Html msg
        renderAccordion =
            List.foldl
                (\renderable ->
                    case renderable of
                        Item i ->
                            Tuple.mapFirst ((::) i)

                        Var v ->
                            Tuple.mapSecond ((::) (css [ Layout.toProperty v ]))

                        Class c ->
                            Tuple.mapSecond ((::) (class c))
                )
                ( [], [] )
                >> (\( items, vars ) ->
                        Keyed.ul
                            (class "Accordion" :: vars ++ classes)
                            (List.sortBy Tuple.first items)
                   )
    in
    config.tree
        |> Tree.mapByPosition Tuple.pair
        |> Tree.view
            (Tree.Uniform renderTree
                { toHtml =
                    createRegions
                        >> List.concatMap renderRegion
                        >> renderAccordion
                }
            )


type alias A msg =
    ( Position, Segment msg )


type alias B msg =
    { orientation : Orientation, role : Role, here : A msg, nest : List (A msg), left : List (A msg), right : List (A msg), down : List (A msg) }


type alias C msg =
    { up : List (A msg), left : List (A msg), x : List (A msg), here : A msg, nest : List (A msg), y : List (A msg), right : List (A msg), down : List (A msg) }


{-| -}
renderBranch : Branch.Fold {} (A msg) (B msg)
renderBranch =
    { init =
        \(( position, segment ) as here) ->
            { orientation = segment.orientation, role = position.role, here = here, nest = [], left = [], right = [], down = [] }
    , grow =
        let
            nest inner b =
                ( inner, { b | nest = b.nest ++ inner.left ++ inner.down ++ inner.right ++ inner.nest } )
        in
        { downwards =
            \(( _, segment ) as a) b ->
                case segment.orientation of
                    Horizontal ->
                        { b | right = b.right ++ [ a ] }

                    Vertical ->
                        { b | down = b.down ++ [ a ] }
        , leftwards =
            \inner ->
                nest inner
                    >> (\( { orientation, here }, b ) ->
                            case orientation of
                                Horizontal ->
                                    { b | left = here :: b.left }

                                Vertical ->
                                    { b | down = here :: b.down }
                       )
        , rightwards =
            \inner ->
                nest inner
                    >> (\( { orientation, here }, b ) ->
                            case orientation of
                                Horizontal ->
                                    { b | right = here :: b.left }

                                Vertical ->
                                    { b | down = b.down ++ [ here ] }
                       )
        }
    }


renderTree : Tree.Fold {} (A msg) (B msg) (C msg)
renderTree =
    let
        nest inner c =
            ( inner, { c | nest = c.nest ++ inner.left ++ inner.down ++ inner.right ++ inner.nest } )
    in
    { init =
        \branch ->
            nest branch
                { up = []
                , left = []
                , x = []
                , here = branch.here
                , nest = []
                , y = []
                , right = []
                , down = []
                }
                |> Tuple.second
    , branch = renderBranch
    , grow =
        { upwards =
            \(( _, segment ) as a) c ->
                case segment.orientation of
                    Horizontal ->
                        { c | left = c.left ++ [ a ] }

                    Vertical ->
                        { c | up = c.up ++ [ a ] }
        , leftwards =
            \inner ->
                nest inner
                    >> (\( branch, c ) ->
                            case ( branch.role, branch.orientation ) of
                                ( Aisle, _ ) ->
                                    { c | x = c.x ++ [ branch.here ] }

                                ( _, Horizontal ) ->
                                    { c | left = c.left ++ [ branch.here ] }

                                ( _, Vertical ) ->
                                    { c | up = c.up ++ [ branch.here ] }
                       )
        , rightwards =
            \inner ->
                nest inner
                    >> (\( branch, c ) ->
                            case ( branch.role, branch.orientation ) of
                                ( Aisle, _ ) ->
                                    { c | y = c.y ++ [ branch.here ] }

                                ( _, Horizontal ) ->
                                    { c | right = c.right ++ [ branch.here ] }

                                ( _, Vertical ) ->
                                    { c | down = c.down ++ [ branch.here ] }
                       )
        }
    }
