module Accordion exposing
    ( Accordion
    , create
    , exit, mapTemplates
    , Msg, update
    , Action(..), decodeAction, encodeAction
    , goToId, goToParentId
    , parentId, focusId
    , isRoot
    , closestId
    , view
    )

{-|


## To Do

  - [ ] Check `exit` behavior (mainly pertains to Main)

---

@docs Accordion


# Create

@docs create


# Modify

@docs exit, mapTemplates


# Update

@docs Msg, update


# Persist

@docs Action, decodeAction, encodeAction


# Navigate

@docs find, goToId, goToParentId


# Deconstruct

@docs parentId, focusId

@docs isRoot


# Query

@docs closestId


# View

@docs view

-}

import Accordion.Segment as Segment exposing (Orientation(..), Segment)
import Accordion.Segment.ViewMode as ViewSegment exposing (Region(..), ViewMode, Width(..))
import Css exposing (..)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class, classList, css, id)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout
import Levenshtein
import List.Extra as List
import String exposing (left)
import Time
import Url exposing (Url)
import Zipper
import Zipper.Branch as Branch
import Zipper.Tree as Tree exposing (Edge(..), EdgeOperation(..), Tree, Walk(..))


{-|

  - `tree`: the result of the Action log
  - `templates`: volatile switchable presets for editing

-}
type Accordion msg
    = Accordion { tree : Tree Segment, templates : Segment.Templates }


{-| -}
singleton : Tree Segment -> Accordion msg
singleton tree =
    Accordion { tree = tree, templates = Segment.initialTemplates }


{-| -}
mapTemplates : (Segment.Templates -> Segment.Templates) -> Accordion msg -> Accordion msg
mapTemplates fu (Accordion config) =
    Accordion { config | templates = fu config.templates }


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


{-| The focus is on a root node
-}
isRoot : Accordion msg -> Bool
isRoot (Accordion config) =
    Tree.isRoot config.tree


{-| Go upmost
-}
root : Accordion msg -> Accordion msg
root (Accordion config) =
    Accordion { config | tree = Tree.root config.tree }



---- ids


{-| Parent id; if is root, then !!
-}
parentId : Accordion msg -> String
parentId (Accordion config) =
    if Tree.isRoot config.tree then
        "" |> Debug.log ("Location: Root / " ++ .id (Tree.focus config.tree))

    else
        config.tree |> Tree.up |> Tree.focus |> .id |> (++) "#" |> Debug.log ("Location: / " ++ .id (Tree.focus config.tree) ++ " \\")


{-| Focus id
-}
focusId : Accordion msg -> String
focusId (Accordion config) =
    config.tree |> Tree.focus |> .id



---- A C T I O N S ----


{-| Serialize the creation of an `Accordion`
-}
type Action
    = Name String
    | Modify Segment.Action
    | Find String
    | Go Direction
    | Insert Direction


{-| -}
decodeAction : Decoder Action
decodeAction =
    Decode.field "Constructor"
        Decode.string
        |> Decode.andThen
            (\constructor ->
                case constructor of
                    "Name" ->
                        Decode.map
                            Name
                            (Decode.field "A1" Decode.string)

                    "Modify" ->
                        Decode.map
                            Modify
                            (Decode.field "A1" Segment.decodeAction)

                    "Find" ->
                        Decode.map
                            Find
                            (Decode.field "A1" Decode.string)

                    "Go" ->
                        Decode.map
                            Go
                            (Decode.field "A1" Fold.decodeDirection)

                    "Insert" ->
                        Decode.map
                            Insert
                            (Decode.field "A1" Fold.decodeDirection)

                    other ->
                        Decode.fail <| "Unknown constructor for type Action: " ++ other
            )


{-| -}
encodeAction : Action -> Value
encodeAction a =
    case a of
        Name a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Name" )
                , ( "A1", Encode.string a1 )
                ]

        Modify a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Modify" )
                , ( "A1", Segment.encodeAction a1 )
                ]

        Find a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Find" )
                , ( "A1", Encode.string a1 )
                ]

        Go a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Go" )
                , ( "A1", Fold.encodeDirection a1 )
                ]

        Insert a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Insert" )
                , ( "A1", Fold.encodeDirection a1 )
                ]


{-| -}
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


{-| The `Url` fragment encodes the parent of the focus!
-}
goToParentId : Maybe String -> Accordion msg -> Accordion msg
goToParentId fragment =
    let
        debugLocation =
            \accordion -> parentId accordion |> always accordion

        goToId_ =
            \str -> .id >> (==) str |> Tree.Find |> Tree.go
    in
    case Debug.log "Accordion tries to find" fragment of
        Nothing ->
            mapTree Tree.root

        Just parent ->
            debugLocation
                >> mapTree (goToId_ parent >> Tree.go (Walk Down (Fail identity)))


{-| -}
goToId : String -> Accordion msg -> Accordion msg
goToId id =
    mapTree (.id >> (==) id |> Tree.Find |> Tree.go)


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


{-| can be used to generate links, for example in Toc or Search
-}
closestId : String -> Accordion msg -> String
closestId searchString (Accordion { tree }) =
    Tree.flatten tree
        |> List.minimumBy (.id >> Levenshtein.distance searchString)
        |> Maybe.map .id
        |> Maybe.withDefault ""


mapSegment : (Segment -> Segment) -> Accordion msg -> Accordion msg
mapSegment =
    Tree.mapFocus >> mapTree



---- View ----


type Renderable msg
    = Item ( String, Html msg )
    | Var ( String, Int )
    | Class String


type alias ViewMode msg =
    { zone : Maybe Time.Zone
    , do : Action -> msg
    , volatile : Msg -> msg
    }


{-| -}
type Msg
    = TemplatesUpdated (Segment.Templates -> Segment.Templates)


{-| -}
update : Msg -> Accordion msg -> Accordion msg
update msg (Accordion config) =
    case msg of
        TemplatesUpdated fu ->
            Accordion { config | templates = fu config.templates }


{-| -}
view : ViewMode msg -> Accordion msg -> Html msg
view { zone, do, volatile } (Accordion config) =
    let
        viewSegment =
            Segment.edit { do = Modify >> do, insert = Insert >> do, templates = config.templates, updateTemplates = TemplatesUpdated >> volatile, context = Tree.split config.tree }

        classes : Html.Attribute msg
        classes =
            classList
                [ ( "\u{1FA97}" ++ Segment.orientationToString (Segment.orientation (Tree.focus config.tree)), True )
                , ( "aisleHasBody", List.any (Segment.hasBody config) (Tree.getAisleNodes config.tree |> Zipper.flat) )
                , ( "focusHasBody", Segment.hasBody config (Tree.focus config.tree) )
                , ( "focusIsRoot", Tree.isRoot config.tree )
                , ( "focusIsBackground", Segment.isBackground (Tree.focus config.tree) )
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
            { targetId = peekParent.id, hint = Segment.hint peekParent }

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
                                    if Segment.isIllustration { templates = config.templates } seg then
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
                    ( ViewSegment.addWidth mode (Segment.hasBody config segment) (Segment.width segment) (Segment.infoLineCount segment) offset
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
            { orientation = Segment.orientation segment, role = position.role, here = here, nest = [], left = [], right = [], down = [] }
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
                case Segment.orientation segment of
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
