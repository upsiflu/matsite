module Accordion exposing
    ( Accordion
    , exit
    , find, root
    , location, focus
    , view
    , anarchiveX, vimeoX
    , Action(..), Generator(..), create, isRoot, renderBranch
    )

{-|


## To Do

  - [ ] Each `view` produces all Segments, in order of the alphabet
  - [ ] `flip` simply goes up one level

---

@docs Accordion
@docs site


# Modify

@docs exit


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
import Levenshtein
import List.Extra as List
import Snippets.Artist as Artist
import String exposing (left)
import Time
import Url exposing (Url)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Mixed as MixedZipper exposing (MixedZipper)
import Zipper.Tree as Tree exposing (Edge(..), EdgeOperation(..), Tree, Walk(..))


{-| -}
type Accordion msg
    = Accordion { tree : Tree Segment, collapsed : Bool }


{-| -}
singleton : Tree Segment -> Accordion msg
singleton tree =
    Accordion { tree = tree, collapsed = False }


{-| -}
exit : Accordion msg -> Accordion msg
exit =
    mapTree
        (\tree ->
            if Tree.isRoot tree then
                tree

            else
                Tree.up tree
        )


isRoot : Accordion msg -> Bool
isRoot (Accordion config) =
    Tree.isRoot config.tree


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
        "" |> Debug.log ("Location: Root / " ++ .id (Tree.focus config.tree))

    else
        config.tree |> Tree.up |> Tree.focus |> .id |> (++) "#" |> Debug.log ("Location: / " ++ .id (Tree.focus config.tree) ++ " \\")


{-| -}
focus : Accordion msg -> String
focus (Accordion config) =
    if Tree.isRoot config.tree then
        ""

    else
        config.tree |> Tree.focus |> .id



---- A C T I O N S ----


{-| Serialize the creation of an Accordion
-}
type Action
    = Name String
    | Modify Segment.Action
    | Generate Generator
    | Find String
    | Go Direction
    | Insert Direction


type Generator
    = Toc


generate : Generator -> Accordion msg -> List Action
generate g =
    case g of
        Toc ->
            \accordionWithArtists ->
                [ Modify <| Segment.WithInfo <| Artist.toc (createLink accordionWithArtists) ]


create : List Action -> Accordion msg
create =
    let
        applyAction : Action -> Accordion msg -> Accordion msg
        applyAction a =
            case a of
                Name caption ->
                    Segment.singleton caption
                        |> setSegment

                Modify segmentAction ->
                    Segment.apply segmentAction
                        |> mapSegment

                Generate generator ->
                    \input ->
                        generate generator input
                            |> List.foldl applyAction input

                Find searchString ->
                    \input ->
                        goTo (closestId searchString input) input

                Go direction ->
                    go direction

                Insert direction ->
                    insert direction

        empty : Accordion msg
        empty =
            Tree.singleton Segment.empty
                |> singleton
    in
    List.foldl applyAction empty



---- Navigate -----


{-| The Url encodes the parent of the focus!
-}
find : Url -> Accordion msg -> Accordion msg
find { fragment } =
    let
        debugLocation =
            \accordion -> location accordion |> always accordion

        goToId =
            \str -> .id >> (==) str |> Tree.Find |> Tree.go
    in
    case Debug.log "Accordion tries to find" fragment of
        Nothing ->
            mapTree Tree.root

        Just parent ->
            debugLocation
                >> mapTree (goToId parent >> Tree.go (Walk Down (Fail identity)))
                >> reset


{-| -}
mapTree : (Tree Segment -> Tree Segment) -> Accordion msg -> Accordion msg
mapTree fu (Accordion config) =
    Accordion { config | tree = fu config.tree }


go : Direction -> Accordion msg -> Accordion msg
go direction =
    Branch.singleton Segment.empty |> Tree.Insert |> Walk direction |> Tree.go |> mapTree


insert : Direction -> Accordion msg -> Accordion msg
insert direction =
    Tree.insert direction Segment.empty |> mapTree


goTo : String -> Accordion msg -> Accordion msg
goTo id =
    .id >> (==) id |> Tree.Find |> Tree.go |> mapTree


set : Orientation -> String -> Segment.Body -> Accordion msg -> Accordion msg
set orientation caption body =
    Segment.singleton caption
        |> Segment.withOrientation orientation
        |> Segment.withBody body
        |> setSegment


setSegment : Segment -> Accordion msg -> Accordion msg
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


createLink : Accordion msg -> String -> Html.Attribute Never
createLink accordion string =
    closestId string accordion
        |> (\id -> href ("#" ++ id))


closestId : String -> Accordion msg -> String
closestId searchString (Accordion { tree }) =
    Tree.flatten tree
        |> List.minimumBy (.id >> Levenshtein.distance searchString)
        |> Maybe.map .id
        |> Maybe.withDefault ""


mapSegment : (Segment -> Segment) -> Accordion msg -> Accordion msg
mapSegment =
    Tree.mapFocus >> mapTree


{-| -}
anarchiveX : Segment.Body
anarchiveX =
    Html.div [ class "anArchive" ]
        [ Html.iframe
            [ attribute "width" "100%"
            , css [ position absolute, Css.height (pct 100), border (px 0) ]
            , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
            , title "Moving Across Thresholds - AnArchive"
            ]
            []
        ]
        |> Segment.Content
        |> Segment.Preset


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


type alias ViewMode msg =
    { zone : Maybe Time.Zone
    , do : Action -> msg
    }


{-| -}
view : ViewMode msg -> Accordion msg -> Html msg
view { zone, do } (Accordion config) =
    let
        viewSegment =
            Segment.edit { do = Modify >> do, insert = Insert >> do }

        classes : Html.Attribute msg
        classes =
            classList
                [ ( "\u{1FA97}" ++ Segment.orientationToString (.orientation (Tree.focus config.tree)), True )
                , ( "aisleHasBody", List.any Segment.hasBody (Tree.getAisleNodes config.tree |> Zipper.flat) )
                , ( "focusHasBody", Segment.hasBody (Tree.focus config.tree) )
                , ( "focusIsRoot", Tree.isRoot config.tree )
                , ( "focusIsBackground", .isBackground (Tree.focus config.tree) )
                ]

        findPeekConfig : Segment -> { targetId : String, hint : String }
        findPeekConfig seg =
            let
                peekParent =
                    .id
                        >> (==) seg.id
                        |> Tree.Find
                        |> Tree.go
                        |> (|>) config.tree
                        |> Tree.up
                        |> Tree.focus
            in
            { targetId = peekParent.id, hint = String.join ", " peekParent.caption }

        createRegions : C -> List ( Region, List A )
        createRegions { up, left, x, here, nest, y, right, down } =
            let
                ( perhapsPeek, cache ) =
                    let
                        findPeek toCache toTest =
                            case toTest of
                                [] ->
                                    Result.Err ()

                                (( _, seg ) as a) :: rest ->
                                    if Segment.isIllustration seg then
                                        Result.Ok ( ( Peek (findPeekConfig seg), [ a ] ), toCache ++ rest )

                                    else
                                        findPeek (a :: toCache) rest
                    in
                    findPeek [] nest
                        |> Result.withDefault
                            ( ( ViewSegment.defaultPeek
                              , [ ( Fold.fataMorganaPosition, Segment.defaultIllustration ) ]
                              )
                            , nest
                            )
            in
            [ ( North, List.reverse up )
            , ( West, List.reverse left )
            , ( NearWest, List.reverse x )
            , ( Center, [ here ] )
            , perhapsPeek
            , ( Cache, cache )
            , ( NearEast, y )
            , ( East, right )
            , ( South, down )
            ]

        renderRegion : ( Region, List A ) -> List (Renderable msg)
        renderRegion ( region, list ) =
            List.foldl
                (\( position, segment ) ( offset, newList ) ->
                    let
                        mode =
                            { zone = zone, position = position, region = region, offset = offset }
                    in
                    ( ViewSegment.addWidth mode (Segment.hasBody segment) segment offset
                    , viewSegment mode segment :: newList
                    )
                )
                ( ViewSegment.zeroOffset, [] )
                list
                |> (\( totalOffset, renderedSegments ) ->
                        ViewSegment.offsetToCssVariables totalOffset
                            |> List.map (Tuple.mapFirst ((++) (ViewSegment.regionToString region ++ "-")) >> Var)
                            |> (++) (List.map Item renderedSegments)
                   )

        overlays : List ( String, Html msg )
        overlays =
            [ ( "screenBackground", Html.div [ class "screenBackground" ] [] )
            , ( "aisleBackground", Html.div [ class "aisleBackground" ] [] )
            , ( "hamburgerMenu", Layout.hamburgerMenu "" )
            ]

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
                            (class "Accordion" :: classes :: vars)
                            (List.sortBy Tuple.first overlays ++ items)
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


type alias A =
    ( Position, Segment )


type alias B =
    { orientation : Orientation, role : Role, here : A, nest : List A, left : List A, right : List A, down : List A }


type alias C =
    { up : List A, left : List A, x : List A, here : A, nest : List A, y : List A, right : List A, down : List A }


{-| assigns sub-nodes to `left`, `right` and `down` while nesting sub-trees, thus `nest` contains all collapsed DOM nodes
in no particular order.
All sub-branches are carried over.
-}
renderBranch : Branch.Fold {} A B
renderBranch =
    let
        {- Keep the sub-trees around in the DOM, even if they don't appear in any region, to allow for smooth transitions -}
        nest inner b =
            ( inner, { b | nest = b.nest ++ inner.left ++ inner.down ++ inner.right ++ inner.nest } )
    in
    { init =
        \(( position, segment ) as here) ->
            { orientation = segment.orientation, role = position.role, here = here, nest = [], left = [], right = [], down = [] }
    , grow =
        { downwards =
            \a b ->
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
                                    { b | right = here :: b.right }

                                Vertical ->
                                    { b | down = b.down ++ [ here ] }
                       )
        }
    }


renderTree : Tree.Fold {} A B C
renderTree =
    let
        {- Keep the sub-trees around in the DOM, even if they don't appear in any region, to allow for smooth transitions -}
        nest : B -> C -> ( B, C )
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
