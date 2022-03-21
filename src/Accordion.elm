module Accordion exposing
    ( Accordion
    , site
    , flip, find
    , location, focus
    , view
    , anarchiveX, vimeoX
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

@docs view

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
    if Tree.isRoot config.tree then
        Debug.log "Weirdly, this tree's focus is on root" ""

    else
        config.tree |> Tree.focus |> .id |> Debug.log "This tree is not on root"


{-| The Url encodes the parent of the focus!
-}
find : Url -> Accordion msg -> Accordion msg
find { fragment } =
    let
        goToId =
            \str -> .id >> (==) str |> Find |> Tree.go
    in
    case Debug.log "Find path" fragment of
        Just parent ->
            mapTree (goToId parent >> Tree.go (Walk Down (Fail identity)))
                >> reset

        Nothing ->
            mapTree Tree.root |> Debug.log "Sadly, there was no fragment."


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

        description =
            horizontalSegment "Description"
                |> setBody Festival.description
                |> noCaption

        video =
            Segment.singleton ""
                |> Segment.withOrientation Horizontal
                |> Segment.withAdditionalAttributes [ class "grow" ]
                |> Segment.withBody Festival.video
                |> Branch.singleton

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
toHtml2 : Orientation -> C msg -> Html msg
toHtml2 orientation { up, left, x, here, y, right, down } =
    let
        addOffsets : String -> List (R msg) -> List (R msg)
        addOffsets side =
            List.indexedMap (\offset -> Cls.withAttributes [ class side, css [ Css.property "--offset" (String.fromInt offset) ] ])

        north =
            addOffsets "north" up
                |> List.reverse

        west =
            addOffsets "west" left
                |> List.reverse

        nearWest =
            addOffsets "nearWest" x
                |> List.reverse

        center =
            [ Cls.withAttributes [ class "here" ] here ]

        nearEast =
            addOffsets "nearEast" y

        east =
            addOffsets "east" right

        south =
            addOffsets "south" down
    in
    [ north, west, nearWest, center, nearEast, east, south ]
        |> List.concat
        |> List.map (Cls.viewWith [ class ("(" ++ Segment.orientationToString orientation ++ ")") ])
        |> Html.div [ class "Accordion2" ]


{-| -}
toHtml : Orientation -> C msg -> Html msg
toHtml orientation { up, left, x, here, y, right, down } =
    let
        direction =
            case orientation of
                Horizontal ->
                    row

                Vertical ->
                    column

        north =
            List.reverse up |> List.map Cls.view

        west =
            List.reverse left |> List.map Cls.view

        center =
            List.reverse x
                ++ here
                :: y
                |> List.map Cls.view
                |> Html.div [ class "Center", css [ displayFlex, flexDirection direction ] ]

        east =
            List.map Cls.view right

        south =
            List.map Cls.view down

        maxLenght =
            Basics.max (List.length west) (List.length east) |> (*) 4 |> toFloat

        withSymmetricWidth className =
            Html.div [ class className, css [ Css.width (rem maxLenght), flexShrink zero, displayFlex, flexDirection row ] ]

        present =
            Html.div [ class "Present", css [ displayFlex, flexDirection row ] ]
                [ withSymmetricWidth "west" west, center, withSymmetricWidth "east" east ]
    in
    north
        ++ present
        :: south
        |> Html.div [ class "Accordion1", css [ displayFlex, flexDirection column ] ]


{-| -}
view : Accordion msg -> Html msg
view (Accordion config) =
    let
        viewMode =
            if config.collapsed then
                Collapsed

            else
                Default

        focalSegment =
            Tree.focus config.tree
    in
    config.tree
        |> Tree.zipDirections
        |> Tree.map (\( path, segment ) -> ( viewMode path, segment ))
        |> Tree.view
            (Tree.Uniform renderTree { toHtml = toHtml focalSegment.orientation })


type alias A msg =
    ( ViewSegment.ViewMode, Segment msg )


type alias R msg =
    Att (Html msg)


renderSegment : ViewSegment.ViewMode -> Segment msg -> R msg
renderSegment mode =
    Cls.create (Segment.view mode)


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
