module Accordion exposing
    ( Accordion
    , Action(..)
    , exit
    , find, root
    , location, focus
    , view
    , anarchiveX, vimeoX
    , isRoot, renderBranch
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
    = Accordion { tree : Tree (Segment), collapsed : Bool }


{-| -}
singleton : Tree (Segment) -> Accordion msg
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


{-| Serialize the creation of an Accordion -}
type Action
    = Name String
    | Modify Segment.Action
    | Generate Generator
    | Find String
    | Go Direction



type Generator
    = Toc

generate : Generator -> Accordion msg -> List Action
generate g =
    case g of
        Toc ->
            (\accordionWithArtists -> 
                [ Modify Segment.withInfo <| Artist.toc (createLink accordionWithArtists) ] )


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
                            |> List.foldl ( (>>) applyAction ) input

                Find searchString ->
                    closestId searchString
                        |> goTo

                Go direction ->
                    go direction`


        empty : Accordion msg 
        empty =
            Tree.singleton Segment.empty
                |> singleton

    in
    List.foldl
        ( (>>) applyAction)
        empty


---- Navigate -----



{-| The Url encodes the parent of the focus!
-}
find : Url -> Accordion msg -> Accordion msg
find { fragment } =
    let
        debugLocation =
            \accordion -> location accordion |> always accordion

        goToId =
            \str -> .id >> (==) str |> Find |> Tree.go
    in
    case Debug.log "Accordion tries to find" fragment of
        Nothing ->
            mapTree Tree.root

        Just parent ->
            debugLocation
                >> mapTree (goToId parent >> Tree.go (Walk Down (Fail identity)))
                >> reset


{-| -}
mapTree : (Tree (Segment) -> Tree (Segment)) -> Accordion msg -> Accordion msg
mapTree fu (Accordion config) =
    Accordion { config | tree = fu config.tree }


go : Direction -> Accordion msg -> Accordion msg
go direction =
    Branch.singleton Segment.empty |> Insert |> Walk direction |> Tree.go |> mapTree

goTo id =
    .id >> (==) id |> Find |> Tree.go |> mapTree


set : Orientation -> String -> Segment.Body msg -> Accordion msg -> Accordion msg
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


createLink : Accordion msg -> String -> Html.Attribute msg
createLink accordion string =
    closestId string accordion
        |> \id -> href ("#" ++ id)

closestId : String -> Accordion msg -> String
closestId searchString =
    tree
        >> Tree.flatten
        >> List.minimumBy (.id >> Levenshtein.distance searchString)
        >> Maybe.map .id
        >> Maybe.withDefault ""


tree : Accordion msg -> Tree Segment
tree (Accordion { tree }) = tree



mapSegment : (Segment -> Segment) -> Accordion msg -> Accordion msg
mapSegment =
    Tree.mapFocus >> mapTree

        



{-| -}
site : Accordion msg
site =
    let
        artists : List (Accordion msg -> Accordion msg)
        artists =
            Artist.artists
                |> List.map
                    (\({ name, wide } as artist) ->
                        set Horizontal (name ++ "(photo)") (Artist.viewPhoto artist)
                            >> (if wide then
                                    mapSegment Segment.increaseColumnCount

                                else
                                    identity
                               )
                            >> go Right
                            >> set Horizontal
                                name
                                (Artist.view artist)
                            >> mapSegment (Segment.withAdditionalAttributes [ class "fg" ])
                            >> go Right
                    )

        doArtists : Accordion msg -> Accordion msg
        doArtists =
            List.foldl (<<) identity artists

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
                >> set Horizontal "Info" Segment.None
                >> go Right
                >> set Horizontal "Collage" Festival.collage
                >> go Right
                >> set Horizontal "Description" Festival.description
                >> go Right
                >> set Horizontal "Video" Festival.video
                >> mapSegment Segment.increaseColumnCount
                >> go Right
                >> set Horizontal "Credits" Segment.None
                >> go Left
                >> go Left
                >> go Up
                >> go Up

        subtreeForLabs =
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
    in
    
        |> set Vertical "Home" Intro.intro
        |> mapSegment (Segment.withBackground True)
        |> go Right
        |> set Vertical "Labs" Segment.None
        |> mapSegment (Segment.withInfo <| Html.text "Biweekly on Thursdays; 90mins")
        |> subtreeForLabs
        |> go Right
        |> set Vertical "Festivals" Segment.None
        |> mapSegment (Segment.withInfo <| Html.text "Text line - Festivals!")
        |> appendSubtree
        |> go Right
        |> set Vertical "Artists" Segment.None
        |> go Down
        |> doArtists
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Up
        |> (\accordionWithArtists -> mapSegment (Segment.withInfo <| Artist.toc (createLink accordionWithArtists)) accordionWithArtists)
        |> go Right
        |> set Vertical "Traces" Segment.None
        |> go Right
        |> set Vertical "Videos" Segment.None
        |> go Right
        |> set Vertical "Library" anarchiveX
        |> go Right
        |> set Vertical "About" Segment.None
        |> go Right
        |> set Vertical "Newsletter" Segment.None
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> go Left
        |> root


{-| -}
anarchiveX : Segment.Body msg
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
        classes : Html.Attribute msg
        classes =
            classList
                [ ( "\u{1FA97}" ++ Segment.orientationToString (.orientation (Tree.focus config.tree)), True )
                , ( "aisleHasBody", List.any (.body >> (/=) Segment.None) (Tree.getAisleNodes config.tree |> Zipper.flat) )
                , ( "focusHasBody", (.body >> (/=) Segment.None) (Tree.focus config.tree) )
                , ( "focusIsRoot", Tree.isRoot config.tree )
                , ( "focusIsBackground", .isBackground (Tree.focus config.tree) )
                ]

        findPeekConfig : Segment -> { targetId : String, hint : String }
        findPeekConfig seg =
            let
                peekParent =
                    .id
                        >> (==) seg.id
                        |> Find
                        |> Tree.go
                        |> (|>) config.tree
                        |> Tree.up
                        |> Tree.focus
            in
            { targetId = peekParent.id, hint = String.join ", " peekParent.caption }

        createRegions : C msg -> List ( Region, List (A msg) )
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

        renderRegion : ( Region, List (A msg) ) -> List (Renderable msg)
        renderRegion ( region, list ) =
            List.foldl
                (\( position, segment ) ( offset, newList ) ->
                    let
                        mode =
                            { position = position, region = region, offset = offset }
                    in
                    ( ViewSegment.addWidth mode (Segment.hasBody segment) segment offset
                    , Segment.view mode segment :: newList
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


type alias A msg =
    ( Position, Segment )


type alias B msg =
    { orientation : Orientation, role : Role, here : A msg, nest : List (A msg), left : List (A msg), right : List (A msg), down : List (A msg) }


type alias C msg =
    { up : List (A msg), left : List (A msg), x : List (A msg), here : A msg, nest : List (A msg), y : List (A msg), right : List (A msg), down : List (A msg) }


{-| assigns sub-nodes to `left`, `right` and `down` while nesting sub-trees, thus `nest` contains all collapsed DOM nodes
in no particular order.
All sub-branches are carried over.
-}
renderBranch : Branch.Fold {} (A msg) (B msg)
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


renderTree : Tree.Fold {} (A msg) (B msg) (C msg)
renderTree =
    let
        {- Keep the sub-trees around in the DOM, even if they don't appear in any region, to allow for smooth transitions -}
        nest : B msg -> C msg -> ( B msg, C msg )
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
