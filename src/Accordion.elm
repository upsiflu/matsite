module Accordion exposing
    ( Accordion
    , site
    , flip, find
    , view
    , anarchiveX, vimeoX
    , location
    )

{-|

@docs Accordion
@docs site


# Map

@docs flip, find


# View

@docs view

---

@docs anarchiveX, vimeoX

-}

import Accordion.Renderable as Renderable exposing (Renderable)
import Accordion.Segment as Segment exposing (Orientation(..), Segment)
import Accordion.Segment.ViewMode as ViewSegment
import Css exposing (..)
import Fold exposing (Direction(..), Foldr)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Snippets.Festival as Festival
import Url exposing (Url)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
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
location : Accordion msg -> String
location (Accordion config) =
    Tree.focus config.tree |> .id


{-| -}
find : Url -> Accordion msg -> Accordion msg
find =
    .fragment
        >> Maybe.map (\str -> .id >> (==) str |> Find |> Tree.go |> mapTree)
        >> Maybe.withDefault identity


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
view : Accordion msg -> Html msg
view (Accordion config) =
    let
        myFocus =
            if Debug.log "config" config.collapsed then
                ViewSegment.collapse ViewSegment.focus

            else
                ViewSegment.focus
    in
    config.tree
        |> Tree.view
            (Tree.Custom renderer
                { renderFocus = Tuple.pair myFocus
                , renderPeriphery = Tuple.pair ViewSegment.periphery
                , transformation =
                    Tree.zipDirections
                        >> Tree.map
                            (\( path, ( viewmode, segment ) ) -> ( ViewSegment.setPath path viewmode, segment ))
                        >> Tree.mapSpine (Tuple.mapFirst ViewSegment.makeSpine)
                        >> Tree.mapAisleNodes (Tuple.mapFirst ViewSegment.makeAisle)
                }
            )
        |> List.singleton
        |> Html.div [ css [ backgroundColor (rgb 22 99 11), padding (rem 1) ] ]


type alias Aisle msg =
    List (Renderable msg)


type alias A msg =
    ( ViewSegment.ViewMode, Segment msg )


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

        vertical =
            css [ displayFlex, justifyContent flexStart, alignItems center, flexDirection column ]

        horizontal =
            css [ displayFlex, justifyContent center ]

        ( leftAligned, rightAligned ) =
            ( css [ justifyContent Css.left ], css [ justifyContent Css.right ] )

        ( red, green, blue ) =
            ( rgb 200 20 40, rgb 10 200 80, rgb 20 20 140 )

        ( black, white, yellow ) =
            ( rgb 0 0 0, rgb 255 255 255, rgb 255 255 0 )

        ( orange, cyan, magenta ) =
            ( rgb 250 180 10, rgb 0 180 205, rgb 205 60 180 )

        ( brown, grey ) =
            ( rgb 110 70 20, rgb 90 90 90 )

        debugging =
            True

        bordered color =
            css <|
                if debugging then
                    [ border3 (px 4) solid color ]

                else
                    []

        consTrunk ( prev, ( currentViewMode, currentSegment ) as current, next ) ( prev0, next0 ) =
            --< in any branch, chew towards the node of the branch
            --< in the past, chew towards the present of the tree
            --< We accumulate vertical futures into the trans side of the tuple
            --< : z -> trunk -> trunk
            --< : ( Aisle msg, A msg, Aisle msg ) -> Tuple ( Aisle msg ) -> Tuple ( Aisle msg )
            let
                currentRenderable =
                    [ Renderable.singleton currentSegment ]
                        |> Renderable.div [ bordered green ]
            in
            Renderable.nestAisle currentViewMode <|
                case currentSegment.orientation of
                    Horizontal ->
                        ( prev0
                            ++ [ Renderable.nestMany ViewSegment.placeholder next
                                    ++ prev
                                    ++ Renderable.div [ bordered red ] [ currentRenderable ]
                                    :: next
                                    ++ Renderable.nestMany ViewSegment.placeholder prev
                                    |> Renderable.div [ horizontal, bordered brown ]
                               ]
                        , next0
                        )

                    Vertical ->
                        ( prev0
                            ++ [ prev ++ [ currentRenderable ] |> Renderable.div [ vertical, bordered yellow ] ]
                        , next ++ next0
                        )

        mergeBranch (( currentViewMode, currentSegment ) as current) ( prev, next ) =
            --< in any branch, merge its node with its chewed future
            --< The directions: Down
            --< : a -> trunk -> b
            --< : ( ViewMode, Segment ) -> Tuple ( Aisle msg ) -> ( A msg, Renderable msg)
            let
                subsegments =
                    List.reverse prev
                        ++ next
                        |> Renderable.div [ vertical, bordered cyan ]
            in
            [ Renderable.singleton currentSegment, subsegments ]
                |> Renderable.div [ vertical, bordered blue ]
                |> Renderable.nest currentViewMode
                |> Tuple.pair current

        mergeTree ( ( _, headSegment ), present ) ( prev, next ) =
            --< in the tree, merge its joined present with its chewed context
            --< : zB -> trunk -> result
            --< : (A, Zipper (Renderable msg)) -> (Aisle msg, Aisle msg) -> Html msg
            let
                {-
                   currentWindow2 =
                       (((Renderable.nestMany ViewSegment.placeholder present.right
                           ++ List.reverse present.left
                           ++ present.focus
                         )
                           |> Renderable.div [ bordered magenta, orient headSegment ]
                        )
                           :: present.right
                           ++ Renderable.nestMany ViewSegment.placeholder present.left
                       )
                           |> Renderable.div [ bordered white, orient headSegment ]
                -}
                aisle : List (Renderable msg)
                aisle =
                    Renderable.nestMany ViewSegment.placeholder present.right
                        ++ List.reverse present.left
                        ++ present.focus
                        :: present.right
                        ++ Renderable.nestMany ViewSegment.placeholder present.left

                body : List (Renderable msg)
                body =
                    []

                currentWindow =
                    (Renderable.nestMany ViewSegment.placeholder present.right
                        ++ List.reverse present.left
                        ++ present.focus
                        :: present.right
                        ++ Renderable.nestMany ViewSegment.placeholder present.left
                    )
                        |> Renderable.div [ bordered white, orient headSegment ]
            in
            prev
                ++ currentWindow
                :: next
                |> Renderable.div [ bordered black, vertical ]
                |> List.singleton
                |> Renderable.div [ css [ Css.width (px 14000) ] ]
                |> List.singleton
                |> Renderable.div [ css [ backgroundColor black, overflowX scroll ] ]
                |> Renderable.render ViewSegment.focus
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
            ( List.reverse prev, a, next )
    , joinBranch =
        --< in the present of the tree, join the focused branch with the aisles
        --< : b -> aisle -> aisle -> zB
        --< : (A msg, Renderable msg) -> Aisle -> Aisle -> (A msg, Zipper (Renderable msg))
        \( a, b ) l r ->
            ( a, Zipper.create b l r )
    , consTrunk = consTrunk
    , mergeBranch = mergeBranch
    , mergeTree = mergeTree
    , leaf =
        ( if debugging then
            [ \_ -> Html.text "❦" ]

          else
            []
        , if debugging then
            [ \_ -> Html.text "" ]

          else
            []
        )
    , left =
        [ if debugging then
            \_ -> Html.text "☙"

          else
            \_ -> Html.text ""
        ]
    , right =
        [ if debugging then
            \_ -> Html.text "❧"

          else
            \_ -> Html.text ""
        ]
    }
