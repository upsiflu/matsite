module Accordion exposing
    ( Accordion
    , site
    , flip, find
    , location, focus
    , view
    , anarchiveX, vimeoX
    , Remainder
    )

{-|

@docs Accordion
@docs site


# Map

@docs flip, find


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
import Accordion.Segment.ViewMode as ViewSegment exposing (Role(..), ViewMode(..))
import Css exposing (..)
import Fold exposing (Direction(..), Foldr)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Snippets.Festival as Festival
import String exposing (left)
import Url exposing (Url)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Mixed as MixedZipper exposing (MixedZipper)
import Zipper.Tree as Tree exposing (Edge(..), EdgeOperation(..), Tree, Walk(..))
import Html.Styled.Keyed as Keyed


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
    if Tree.isRoot config.tree then ""

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


{-| -}
insertToTree : Branch (Segment msg) -> Tree (Segment msg) -> Tree (Segment msg)
insertToTree branch tree =
    let
        id =
            Branch.node branch |> .id

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
    Tree.mapBranch (\_ -> branch |> Branch.mapNode (\segment -> { segment | id = uniqueId })) tree


{-| -}
site : Accordion msg
site =
    let
        go direction =
            Tree.go (Walk direction (Insert placeholder))

        set : Branch (Segment msg) -> Tree (Segment msg) -> Tree (Segment msg)
        set =
            insertToTree

        verticalSegment x =
            Segment.singleton x
                |> Segment.withOrientation Vertical
                |> Branch.singleton

        horizontalSegment x =
            Segment.singleton x
                |> Segment.withOrientation Horizontal
                |> Branch.singleton

        setBody =
            Segment.withBody >> Branch.mapNode

        noCaption =
            Segment.withoutCaption |> Branch.mapNode

        emptySegment =
            Segment.empty |> Branch.singleton

        anarchive =
            verticalSegment "Anarchive"

        placeholder =
            emptySegment

        vimeo =
            verticalSegment "Vimeo"

        lab =
            horizontalSegment "Lab"

        festival =
            horizontalSegment "Festival"

        artist =
            horizontalSegment "Artist"

        info =
            horizontalSegment "Info"

        collage =
            horizontalSegment "Collage"
                |> setBody Festival.collage
                |> noCaption

        description =
            horizontalSegment "Description"
                |> setBody Festival.description
                |> noCaption

        video =
            Segment.singleton ""
                |> Segment.withOrientation Horizontal
                |> Segment.increaseColumnCount
                |> Segment.withBody Festival.video
                |> Branch.singleton
                |> noCaption

        credits =
            horizontalSegment "Credits"

        newsletter =
            verticalSegment "Subscribe"

        about =
            verticalSegment "About"

        series =
            String.fromInt >> (++) "Series " >> horizontalSegment

        date =
            verticalSegment

        setDate =
            date >> set

        appendSubtree =
            go Down
                >> setDate "Future Festival - August 22"
                >> go Right
                >> setDate "Future Festival - June 5-19"
                >> go Right
                >> setDate "Foregrounding the background - March 23 + 24"
                >> go Right
                >> setDate "Previous Festival - November 2, 2021"
                >> go Left
                >> go Down
                >> set info
                >> go Right
                >> set collage
                >> go Right
                >> set description
                >> go Right
                >> set video
                >> go Right
                >> set credits
                >> go Left
                >> go Left
                >> go Up
                >> go Up
    in
    Tree.fromBranch anarchive
        |> go Right
        |> set vimeo
        |> go Right
        |> go Right
        |> set newsletter
        |> go Right
        |> set about
        |> go Left
        |> go Left
        |> go Down
        |> set (series 6)
        |> go Left
        |> set (series 5)
        |> go Left
        |> set (series 4)
        |> go Left
        |> set (series 3)
        |> go Right
        |> go Right
        |> go Down
        |> set lab
        |> appendSubtree
        |> go Right
        |> set festival
        |> appendSubtree
        |> go Right
        |> set artist
        |> appendSubtree
        |> go Left
        |> go Down
        --|> go Down
        --|> go Up
        --|> go Right
        |> singleton


{-| -}
anarchiveX : Html msg
anarchiveX =
    Html.iframe
        [ attribute "width" "100%"
        , css [ position absolute, Css.height (vh 100), border (px 0) ]
        , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
        , title "Moving Across Thresholds - AnArchive"
        ]
        []


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


{-| -}
toHtml2 : List (R msg) -> C msg -> (Html msg, Remainder msg)
toHtml2 remainder { up, left, x, here, y, right, down } =
    let
        addOffsets : { area : String, regardColumnCount : Bool } -> List (R msg) -> ( List (R msg), Int )
        addOffsets config =
            List.foldl
                (\( s, r ) ( list, i ) ->
                    let
                        actualColumns =
                            if config.regardColumnCount then
                                s.columnCount

                            else
                                1

                        j =
                            i + actualColumns
                    in
                    ( ( s
                      , Cls.withAttributes
                            [ class config.area
                            , css
                                [ Css.property "--columnCount" (String.fromInt actualColumns)
                                , Css.property "--offset" (String.fromInt i)
                                ]
                            ]
                            r
                      )
                        :: list
                    , j
                    )
                )
                ( [], 0 )

        ---
        ( north, northCount ) =
            addOffsets { area = "north", regardColumnCount = False } up
                |> Tuple.mapFirst List.reverse

        ( west, westCount ) =
            addOffsets { area = "west", regardColumnCount = False } left
                |> Tuple.mapFirst List.reverse

        ( nearWest, nearWestCount ) =
            addOffsets { area = "nearWest", regardColumnCount = True } x
                |> Tuple.mapFirst List.reverse

        ( center, hereCount ) =
            addOffsets { area = "here", regardColumnCount = True } [ here ]

        ( nearEast, nearEastCount ) =
            addOffsets { area = "nearEast", regardColumnCount = True } y

        ( east, eastCount ) =
            addOffsets { area = "east", regardColumnCount = False } right

        ( south, southCount ) =
            addOffsets { area = "south", regardColumnCount = False } down

        sendToCss key value =
            css [ Css.property ("--" ++ key) (String.fromInt value) ]

        orientation =
            Tuple.first >> .orientation

        visibleRs =
            [ north, west, nearWest, center, nearEast, east, south ]
                |> List.map (List.map (Tuple.mapSecond (Cls.withAttributes [ class ("(" ++ Segment.orientationToString (orientation here) ++ ")") ])))
                |> List.concat

        visibleIds =
            List.map (Tuple.first >> .id) visibleRs |> Debug.log ("VISIBLE")

        invisibleRs =
            remainder
                |> List.filter (Tuple.first >> .id >> (\invisibleId -> List.member invisibleId visibleIds) >> not)
                |> List.map (Tuple.mapSecond (Cls.withAttributes [ class ("vanishing") ])) 

        invisibleIds =
            invisibleRs
                |> List.map (Tuple.first >> .id) |> Debug.log ("INVISIBLE")

    in
    visibleRs
        |> (++) invisibleRs
        |> List.sortBy (Tuple.first >> .id)
        |> (\sortedR -> (sortedR, sortedR))
        |> Tuple.mapFirst (List.map (Tuple.second >> Cls.view))
        |> Tuple.mapFirst 
            (   (++) 
                    [ ("_centerBackground", Html.li [class "centerBackground"] [Html.text " "])
                    , ("_hereBackground", Html.li [class "hereBackground"] [Html.text " "])
                    , ("_screenBackground", Html.li [class "screenBackground"] [Html.text " "])
                    , ("_westIndicator", Html.li [class "westIndicator"] [Html.text " "])
                    , ("_eastIndicator", Html.li [class "eastIndicator"] [Html.text " "])
                    ]
                >> Keyed.ul
                    [ class "Accordion2"
                    , sendToCss "northCount" northCount
                    , sendToCss "westCount" westCount
                    , sendToCss "nearWestCount" nearWestCount
                    , sendToCss "hereCount" hereCount
                    , sendToCss "nearEastCount" nearEastCount
                    , sendToCss "eastCount" eastCount
                    , sendToCss "southCount" southCount
                    , class (Segment.orientationToString (orientation here))
                    , classList [("hasBody", List.any (Tuple.first >> Segment.hasBody) (nearWest++center++nearEast))]
                    ] 
            )

type alias Remainder msg = 
    List (R msg)

{-| -}
view : Remainder msg -> Accordion msg -> (Html msg, Remainder msg)
view remainder (Accordion config) =
    let
        viewMode =
            if config.collapsed then
                Collapsed

            else
                Default
    in
    config.tree
        |> Tree.zipDirections
        |> Tree.map (\( path, segment ) -> ( viewMode path, segment ))
        |> Tree.view
            (Tree.Uniform renderTree { toHtml = toHtml2 remainder })


type alias A msg =
    ( ViewSegment.ViewMode, Segment msg )

type alias Keyed msg = 
    (String, Html msg)

type alias R msg =
    ( Segment msg, Att (Keyed msg) )


renderSegment : ViewSegment.ViewMode -> Segment msg -> R msg
renderSegment mode segment =
    ( segment, Cls.create (Segment.view mode) segment )


type alias B msg =
    { orientation : Orientation, role : ViewSegment.Role, here : R msg, nest : List (R msg), left : List (R msg), right : List (R msg), down : List (R msg) }


type alias C msg =
    { up : List (R msg), left : List (R msg), x : List (R msg), here : R msg, nest : List (R msg), y : List (R msg), right : List (R msg), down : List (R msg) }


{-| -}
renderBranch : Branch.Fold {} (A msg) (B msg)
renderBranch =
    { init =
        \( mode, segment ) ->
            { orientation = segment.orientation, role = ViewSegment.role mode, here = renderSegment mode segment, nest = [], left = [], right = [], down = [] }
    , grow =
        let
            nest inner b =
                ( inner, { b | nest = b.nest ++ inner.left ++ inner.down ++ inner.right ++ inner.nest } )
        in
        { downwards =
            \( mode, segment ) b ->
                case segment.orientation of
                    Horizontal ->
                        { b | right = b.right ++ [ renderSegment mode segment ] }

                    Vertical ->
                        { b | down = b.down ++ [ renderSegment mode segment ] }
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
            \( mode, segment ) c ->
                case segment.orientation of
                    Horizontal ->
                        { c | left = c.left ++ [ renderSegment mode segment ] }

                    Vertical ->
                        { c | up = c.up ++ [ renderSegment mode segment ] }
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
