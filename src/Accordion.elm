module Accordion exposing (Accordion, anarchiveX, site, view, vimeoX)

import Accordion.Segment as Segment exposing (Orientation(..), Segment, ViewMode(..))
import Css exposing (..)
import Fold exposing (Foldr)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Snippets.Festival as Festival
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Tree as Tree exposing (Direction(..), Edge(..), EdgeOperation(..), Tree, Walk(..))


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

        setBody =
            Segment.withBody >> Branch.mapNode

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
                |> setBody Festival.description

        video =
            horizontalSegment "Video"
                |> setBody Festival.video

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
        |> go Down
        --|> go Up
        --|> go Right
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
            (Tree.Custom renderer
                { renderFocus = Tuple.pair (Expanded { focused = True } [])
                , renderPeriphery = Tuple.pair (Collapsed [])
                , transformation =
                    Tree.mapTail (Tuple.mapFirst (\_ -> Expanded { focused = False } []))

                -->> Tree.mapAisles ( Tuple.mapFirst (\_-> Expanded {focused = False}) |> Branch.map )
                }
            )
        |> List.singleton
        |> Html.div [ css [ backgroundColor (rgb 22 99 11), padding (rem 5) ] ]


{-| -}
type alias Renderable msg =
    Segment.ViewMode -> Html msg


{-| Like this:
-}
myRenderable : Renderable msg
myRenderable mode =
    Segment.view mode (Segment.singleton "test")


createRenderable : Segment.ViewMode -> Segment msg -> Renderable msg
createRenderable innerMode segment =
    \inheritedMode -> Segment.view (Segment.preferMode inheritedMode innerMode) segment


render : Segment.ViewMode -> Renderable msg -> Html msg
render mode renderable =
    renderable mode


invisible : Renderable msg -> Renderable msg
invisible =
    preferMode Invisible


collapsed : Renderable msg -> Renderable msg
collapsed =
    preferMode (Collapsed [])


focused : Renderable msg -> Renderable msg
focused =
    preferMode (Expanded { focused = True } [])


wrap : (Html msg -> Html msg) -> Renderable msg -> Renderable msg
wrap wrapper renderable =
    \inheritedMode -> render inheritedMode renderable |> wrapper



---- more wrappers


div : List (Html.Attribute msg) -> List (Renderable msg) -> Renderable msg
div attr children =
    \inheritedMode ->
        List.map (render inheritedMode) children
            |> Html.div attr


{-| -}
preferMode : Segment.ViewMode -> Renderable msg -> Renderable msg
preferMode innerMode renderable =
    \inheritedMode -> renderable (Segment.preferMode inheritedMode innerMode)


{-| -}
changeDirection : Direction -> Renderable msg -> Renderable msg
changeDirection innerDirection renderable =
    \inheritedMode -> renderable (Segment.changeDirection innerDirection inheritedMode)


{-| -}
continueDirection : Renderable msg -> Renderable msg
continueDirection renderable =
    \inheritedMode -> renderable (Segment.continueDirection inheritedMode)


repeat i fu target =
    if i < 0 then
        target

    else
        repeat (i - 1) fu target


directAisle : Direction -> Aisle msg -> Aisle msg
directAisle dir =
    List.indexedMap (\i -> repeat (i + 1) (changeDirection dir))


directAislePlus : Int -> Direction -> Aisle msg -> Aisle msg
directAislePlus int dir =
    List.indexedMap (\i -> repeat (i + 1 + int) (changeDirection dir))


type alias Aisle msg =
    List (Renderable msg)


type alias A msg =
    ( Segment.ViewMode, Segment msg )


{-| a _level_ in the past or future
Depending on its orientation, its third element (next) will need to be pushed back
-}
type alias Z msg =
    ( Aisle msg, A msg, Aisle msg )


{-| the _current_ level, tagged by its node
-}
type alias ZB msg =
    ( A msg, Zipper (Renderable msg) )


{-| branch, tagged by its node
-}
type alias B msg =
    ( A msg, Renderable msg )


{-| contains accumulated levels, either "cis" or "trans".
We accumulate vertical futures into the trans side of the tuple
whereas we eagerly render horizontal levels
-}
type alias Trunk msg =
    ( Aisle msg, Aisle msg )


renderer :
    Foldr
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
        (B msg)
        --e:
        (Html msg)
renderer =
    let
        orient { orientation } =
            case orientation of
                Horizontal ->
                    horizontal

                Vertical ->
                    vertical

        dimmed =
            css [ opacity (num 0.1) ]

        asNode =
            css []

        asLeaf =
            css []

        vertical =
            css [ displayFlex, justifyContent flexStart, alignItems center, flexDirection column ]

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

        ( brown, grey ) =
            ( rgb 110 70 20, rgb 90 90 90 )

        debugging =
            False

        bordered color =
            css <|
                if debugging then
                    [ border3 (px 8) solid color ]

                else
                    []

        hideVertically =
            Html.div [ css [ visibility Css.hidden, overflow Css.hidden, maxHeight (px 0) ] ]
    in
    { consAisle =
        --< in any aisle, chew branches
        --< : b -> aisle -> aisle
        --< : (A msg, Renderable msg) -> List (Renderable msg) -> List (Renderable msg)
        Tuple.second >> (::)
    , join =
        --< in any past or future level, join the preferred segment with the aisles
        --< : a -> aisle -> aisle -> z
        --< : (ViewMode, Segment) -> Aisle -> Aisle -> (Aisle msg, (ViewMode, Segment), Aisle msg)
        \a prev next ->
            ( directAisle Left (List.reverse prev), a, directAisle Right next )
    , joinBranch =
        --< in the present of the tree, join the focused branch with the aisles
        --< : b -> aisle -> aisle -> zB
        --< : (A msg, Renderable msg) -> Aisle -> Aisle -> (A msg, Zipper (Renderable msg))
        \( a, b ) l r ->
            ( a, Zipper.create b (directAisle Left l) (directAisle Right r) )
    , consTrunk =
        --< in any branch, chew towards the node of the branch
        --< in the past, chew towards the present of the tree
        --< We accumulate vertical futures into the trans side of the tuple
        --< : z -> trunk -> trunk
        --< : ( Aisle msg, A msg, Aisle msg ) -> Tuple ( Aisle msg ) -> Tuple ( Aisle msg )
        \( prev, ( currentViewMode, currentSegment ), next ) ( prev0, next0 ) ->
            let
                current =
                    createRenderable currentViewMode currentSegment
                        |> wrap (List.singleton >> Html.div [ bordered green ])

                ( pWidth, nWidth ) =
                    ( List.length prev, List.length next )
            in
            case currentSegment.orientation of
                Horizontal ->
                    ( directAislePlus pWidth Left prev0
                        ++ [ List.map invisible next
                                ++ directAisle Left prev
                                ++ [ current ]
                                ++ directAisle Right next
                                ++ List.map invisible prev
                                |> div [ horizontal, bordered brown ]
                           ]
                    , directAislePlus nWidth Right next0
                    )

                Vertical ->
                    ( directAislePlus pWidth Left prev0
                        ++ [ directAisle Left prev ++ [ current ] |> div [ vertical, bordered yellow ] ]
                    , directAisle Right (next ++ next0)
                    )
    , mergeBranch =
        --< in any branch, merge its node with its chewed future
        --< The directions: Down
        --< : a -> trunk -> b
        --< : ( ViewMode, Segment ) -> Tuple ( Aisle msg ) -> ( A msg, Renderable msg)
        \( currentViewMode, currentSegment ) ( prev, next ) ->
            let
                subsegments =
                    List.reverse (directAisle Up prev)
                        ++ directAisle Down next
                        |> div [ vertical, bordered cyan, subStyle ]
                        |> subTransform

                ( subStyle, subTransform ) =
                    case currentViewMode of
                        Expanded _ _ ->
                            ( css [], identity )

                        Collapsed _ ->
                            ( css
                                [ maxWidth zero
                                , maxHeight zero
                                , overflow Css.hidden
                                ]
                            , invisible
                            )

                        Invisible ->
                            ( css [ visibility Css.hidden ], identity )
            in
            createRenderable currentViewMode currentSegment
                :: [ subsegments ]
                |> div [ vertical, bordered blue ]
                |> Tuple.pair ( currentViewMode, currentSegment )
    , mergeTree =
        --< in the tree, merge its joined present with its chewed context
        --< : zB -> trunk -> result
        --< : (A, Zipper (Renderable msg)) -> (Aisle msg, Aisle msg) -> Html msg
        \( a, present ) ( prev, next ) ->
            let
                ( currentViewMode, currentSegment ) =
                    a

                currentWindow =
                    present
                        |> Zipper.flat
                        |> div [ bordered black, orient currentSegment ]
            in
            directAisle Up prev
                ++ [ currentWindow ]
                ++ next
                |> div [ bordered black, vertical ]
                |> List.singleton
                |> div [ css [ Css.width (px 2000) ] ]
                |> List.singleton
                |> div [ css [ backgroundColor black, Css.width (rem 29), overflowX scroll ] ]
                |> render (Expanded { focused = True } [])
    , leaf = ( [ (\_ -> Html.text "ROOT") |> changeDirection Up ], [ (\_ -> Html.text "LEAF") |> changeDirection Down ] )
    , left = [ [ \_ -> Html.text "|<<" ] |> div [] |> changeDirection Left ]
    , right = [ [ \_ -> Html.text ">>|" ] |> div [] |> changeDirection Right ]
    }
