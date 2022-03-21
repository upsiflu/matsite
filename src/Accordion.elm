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
                |> Segment.withAdditionalClasses ["grow"]
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
view : Accordion msg -> Html msg
view (Accordion config) =
    let
        viewMode =
            if config.collapsed then
                Collapsed

            else
                Default

        toHtml : C msg -> Html msg
        toHtml { up, left, x, here, y, right, down } =
            let
                direction =
                    case Tree.focus config.tree |> .orientation of
                        Horizontal ->
                            row

                        Vertical ->
                            column

                north =
                    List.reverse up

                west =
                    List.reverse left

                center =
                    List.reverse x
                        ++ here
                        :: y
                        |> Html.div [ class "Center", css [ displayFlex, flexDirection direction ] ]

                east =
                    right

                south =
                    down

                wLength = List.length west
                eLenght = List.length east
                maxLenght = Basics.max wLength eLenght |> (*) 4 |> toFloat
                withSymmetricWidth className =
                    Html.div [ class className, css [Css.width ((rem maxLenght)), flexShrink zero, displayFlex, flexDirection row] ]

                present =
                    Html.div [ class "Present", css [ displayFlex, flexDirection row ] ]
                        [withSymmetricWidth "west" west, center, withSymmetricWidth "east" east]

            in
            north
                ++ present
                :: south
                |> Html.div [ class "Accordion", css [ displayFlex, flexDirection column ] ]
    in
    config.tree
        |> Tree.zipDirections
        |> Tree.map (\( path, segment ) -> ( viewMode path, segment ))
        |> Tree.view
            (Tree.Uniform renderTree { toHtml = toHtml })


type alias A msg =
    ( ViewSegment.ViewMode, Segment msg )


type alias B msg =
    { orientation : Orientation, role : ViewSegment.Role, here : Html msg, left : List (Html msg), right : List (Html msg), down : List (Html msg) }


type alias C msg =
    { up : List (Html msg), left : List (Html msg), x : List (Html msg), here : Html msg, y : List (Html msg), right : List (Html msg), down : List (Html msg) }


{-| -}
renderBranch : Branch.Fold {} (A msg) (B msg)
renderBranch =
    { init =
        \( mode, segment ) ->
            { orientation = segment.orientation, role = ViewSegment.role mode, here = Segment.view mode segment, left = [], right = [], down = [] }
    , grow =
        { downwards =
            \( mode, segment ) b ->
                case segment.orientation of
                    Horizontal ->
                        { b | right = b.right ++ [ Segment.view mode segment ] }

                    Vertical ->
                        { b | down = b.down ++ [ Segment.view mode segment ] }
        , leftwards =
            \{ orientation, here } b ->
                case orientation of
                    Horizontal ->
                        { b | left = here :: b.left }

                    Vertical ->
                        { b | down = here :: b.down }
        , rightwards =
            \{ orientation, here } b ->
                case orientation of
                    Horizontal ->
                        { b | right = here :: b.left }

                    Vertical ->
                        { b | down = b.down ++ [ here ] }
        }
    }


renderTree : Tree.Fold {} (A msg) (B msg) (C msg)
renderTree =
    { init =
        \{ orientation, here, down } ->
            case orientation of
                Horizontal ->
                    { up = []
                    , left = []
                    , x = []
                    , here = here
                    , y = down
                    , right = []
                    , down = []
                    }

                Vertical ->
                    { up = []
                    , left = []
                    , x = []
                    , here = here
                    , y = down
                    , right = []
                    , down = []
                    }
    , branch = renderBranch
    , grow =
        { upwards =
            \( mode, segment ) c ->
                case segment.orientation of
                    Horizontal ->
                        { c | left = c.left ++ [ Segment.view mode segment ] }

                    Vertical ->
                        { c | up = c.up ++ [Segment.view mode segment] }
        , leftwards =
            \branch c ->
                case ( branch.role, branch.orientation ) of
                    ( Aisle, _ ) ->
                        { c | x = c.x ++ [ branch.here ] }

                    ( _, Horizontal ) ->
                        { c | left = c.left ++ [ branch.here ] }

                    ( _, Vertical ) ->
                        { c | up = c.up ++ [ branch.here ] }
        , rightwards =
            \branch c ->
                case ( branch.role, branch.orientation ) of
                    ( Aisle, _ ) ->
                        { c | y = c.y ++ [ branch.here ] }

                    ( _, Horizontal ) ->
                        { c | right = c.right ++ [ branch.here ] }

                    ( _, Vertical ) ->
                        { c | down = c.down ++ [ branch.here ] }
        }
    }
