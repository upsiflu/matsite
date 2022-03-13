module Accordion exposing (Accordion, view, vimeoX, anarchiveX, site)



import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)

import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Tree as Tree exposing (Direction(..), Edge(..), EdgeOperation(..), Tree, Walk(..))
import Fold exposing (Fold)
import Accordion.Segment as Segment exposing (Segment, Orientation(..), ViewMode(..))

type Accordion msg
    = Accordion (Tree (Segment msg))

singleton : Tree (Segment msg) -> Accordion msg
singleton =
    Accordion







site : Accordion msg
site =
    let
        go direction =
            Tree.go (Walk direction (Insert placeholder))

        set =
            always >> Tree.mapBranch

        verticalSegment x =
            Segment.singleton x 
                |> Segment.withOrientation Vertical
                |> Branch.singleton

        horizontalSegment x =
            Segment.singleton x 
                |> Segment.withOrientation Horizontal
                |> Branch.singleton

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

        description =
            horizontalSegment "Description"

        video =
            horizontalSegment "Video"

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

        setDate = date >> set

        appendSubtree =
            go Down
                >> setDate "March 22"
                >> go Right
                >> setDate "March 24"
                >> go Right
                >> setDate "March 26"
                >> go Right
                >> setDate "April 14"
                >> go Left
                >> go Down
                >> set info
                >> go  Right
                >> set collage
                >> go  Right
                >> set description
                >> go  Right
                >> set video
                >> go  Right
                >> set credits
                >> go  Left
                >> go  Left
                >> go  Up
                >> go  Up
    in
    Tree.fromBranch anarchive
        |> go  Right
        |> set vimeo
        |> go  Right
        |> go  Right
        |> set newsletter
        |> go  Right
        |> set about
        |> go  Left
        |> go  Left
        |> go  Down
        |> set (series 6)
        |> go Left
        |> set (series 5)
        |> go Left
        |> set (series 4)
        |> go Left
        |> set (series 3)
        |> go Right
        |> go Right
        |> go Right
        |> go Down
        |> set lab
        |> appendSubtree
        |> go  Right
        |> set festival
        |> appendSubtree
        |> go  Right
        |> set artist
        |> appendSubtree
        |> go  Left
        |> go  Down
        --|> go  Down
        |> singleton


anarchiveX : Html msg
anarchiveX =
    Html.iframe
        [ attribute "width" "100%"
        , css [ position absolute, Css.height (vh 100), border (px 0) ]
        , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
        , title "Moving Across Thresholds - AnArchive"
        ]
        []


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

view : Accordion msg -> Html msg
view (Accordion tree) =
    tree
        |> Tree.view
            ( Tree.Custom renderer
                { renderFocus = Tuple.pair (Expanded {focused = True})
                , renderPeriphery = Tuple.pair Collapsed
                , finalWalk = Jump Root
                }
            )
        |> List.singleton
        |> Html.div [ css [ backgroundColor (rgb 22 99 11), padding (rem 5) ] ]


type alias Aisle msg = List (Html msg)
type alias A msg = ( Segment.ViewMode, Segment msg )

{-| a _level_ in the past or future 
Depending on its orientation, its third element (next) will need to be pushed back -}
type alias Z msg =
    ( Aisle msg, A msg, Aisle msg )

{-| the _current_ level -}
type alias ZB msg =
    Zipper (Html msg)

{-| contains accumulated levels, either "cis" or "trans".
We accumulate vertical futures into the trans side of the tuple
whereas we eagerly render horizontal levels -}
type alias Trunk msg =
    ( Aisle msg, Aisle msg )



renderer :
    Fold
        --f:
        {}
        --a:
        (A msg)
        --aisle:
        (Aisle msg)
        --z:
        (Z msg)
        ---zB: (List (Html msg))
        (ZB msg)
        --trunk:
        (Trunk msg)
        --b:
        (Html msg)
        --e:
        (Html msg)
renderer =
    let
        orient {orientation} =
            case orientation of
                Horizontal -> horizontal
                Vertical -> vertical

        dimmed =
            css [ opacity (num 0.5) ]

        asNode =
            css []

        asLeaf =
            css []

        focused =
            bordered yellow

        vertical =
            css [ displayFlex, justifyContent center, alignItems center, flexDirection column ]

        horizontal =
            css [ displayFlex, justifyContent center ]

        ( leftAligned, rightAligned ) =
            ( css [ justifyContent Css.left ], css [ justifyContent Css.right ] )

        ( red, green, blue ) =
            ( rgb 200 20 40, rgb 90 240 80, rgb 20 20 140 )

        ( black, white, yellow ) =
            ( rgb 0 0 0, rgb 255 255 255, rgb 255 255 0 )
        
        ( orange, cyan, magenta ) =
            ( rgb 250 180 10, rgb 10 205 205, rgb 205 60 180 )

        bordered color =
            css [ border3 (px 8) solid color ]

        hideVertically =
            Html.div [ css [ visibility Css.hidden, overflow Css.hidden, maxHeight (px 0) ] ]
    in
    { consAisle = 
        --< in any aisle, chew branches
        --< : b -> aisle -> aisle
        --< : Html msg -> List (Html msg) -> List (Html msg)
        (::) 

    , join =
        --< in any past or future level, join the preferred segment with the aisles
        --< : a -> aisle -> aisle -> z
        --< : (ViewMode, Segment) -> List (Html msg) -> List (Html msg) -> (Aisle msg, Segment msg, Aisle msg)
        \a prev next ->
            ( List.reverse prev, a, next )

    , joinBranch =
        --< in the present of the tree, join the focused branch with the aisles
        --< : b -> aisle -> aisle -> zB 
        --< : Html msg -> List (Html msg) -> List (Html msg) -> Zipper (Html msg)
        Zipper.join

    , consTrunk = 
        --< in any branch, chew towards the node of the branch
        --< in the past, chew towards the present of the tree
        --< We accumulate vertical futures into the trans side of the tuple
        --< : z -> trunk -> trunk
        --< : ( Aisle msg, A msg, Aisle msg ) -> Tuple ( Aisle msg ) -> Tuple ( Aisle msg )
        \(prev, ( currentViewMode, currentSegment ), next) ( prev0, next0 ) ->
            let
                current = 
                    [ Segment.view currentViewMode currentSegment ]
                        |> Html.div [ bordered green ]
                        |> List.singleton
            in
            case currentSegment.orientation of
                Horizontal ->
                    let
                        makeInvisible = 
                            List.map
                                (\x-> Html.div [ css [ opacity (num 0.3) ] ] [x])
                        
                    in
                    ( prev0 
                        ++ [ makeInvisible next ++ prev ++ current ++ next  ++ makeInvisible (prev)
                            |> Html.div [ horizontal, bordered white ] 
                            ]
                    , next0 
                    )

                Vertical ->
                    ( prev0 ++ [ prev ++ current |> Html.div [ vertical, bordered yellow ] ]
                    , next ++ next0 
                    )
                    

    , mergeBranch =
        --< in any branch, merge its node with its chewed future
        --< : a -> trunk -> b
        --< : ( ViewMode, Segment ) -> Tuple ( Aisle msg ) -> Html msg
        \( currentViewMode, currentSegment ) ( prev, next ) ->
            let
                subsegments =
                    case currentSegment.orientation of
                        Horizontal ->
                            (prev ++ next)
                            |> Html.div [ horizontal, bordered orange ]

                        Vertical ->
                            (List.reverse prev ++ next)
                            |> Html.div [ vertical, bordered red ]

            in
            (Segment.view currentViewMode currentSegment ) :: [subsegments]
                |> Html.div [vertical, bordered blue ]
            


    , mergeTree =
        --< in the tree, merge its joined present with its chewed context
        --< : zB -> trunk -> result
        --< : Zipper (Html msg) -> (Aisle msg, Aisle msg) -> Html msg
        \present ( prev, next ) ->
            present
                |> Zipper.prepend prev
                |> Zipper.append next
                |> Zipper.flat
                |> Html.div [ vertical, bordered black ] --???
                |> List.singleton
                |> Html.div [ css [ Css.width (px 2000) ] ]
                |> List.singleton
                |> Html.div [ css [ backgroundColor black, Css.width (rem 29), overflowX scroll ] ]

    , leaf = ( [], [] )
    , left = [[Html.text "|<<"] |> Html.div []]
    , right = [[Html.text ">>|"] |> Html.div []]
    }