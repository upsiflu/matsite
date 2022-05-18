module Accordion exposing
    ( Accordion
    , create
    , exit, mapTemplates
    , Msg, update
    , Action(..), actionCodec
    , goToId, goToParentId
    , parentId, focusId
    , isRoot
    , closestId
    , view
    , History, Intent, historyCodec, intentCodec, reviseHistory
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

@docs Action, actionCodec


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
import Codec exposing (Codec, bool, decoder, encoder, field, float, int, maybeField, string, variant0, variant1, variant2)
import Css exposing (..)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class, classList, css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout
import Levenshtein
import List.Extra as List
import String exposing (left)
import Time
import Ui exposing (Ui)
import Zipper
import Zipper.Branch as Branch
import Zipper.Tree as Tree exposing (Edge(..), EdgeOperation(..), Tree, Walk(..))


{-|

  - `tree`: the result of the History, but the `focus` position is volatile and kept even when the server sends a revised History
  - `templates`: volatile switchable presets for editing

-}
type Accordion
    = Accordion Config
    | Log LogConfig Config


type alias Config =
    { tree : Tree Segment, templates : Segment.Templates }


type alias LogConfig =
    { viewingHistory : Bool, history : History }


mapConfig : (Config -> Config) -> Accordion -> Accordion
mapConfig fu accordion =
    case accordion of
        Accordion c ->
            Accordion (fu c)

        Log l c ->
            Log l (fu c)


mapLog : (LogConfig -> LogConfig) -> Accordion -> Accordion
mapLog fu accordion =
    case accordion of
        Log l c ->
            Log (fu l) c

        _ ->
            accordion


config : Accordion -> Config
config accordion =
    case accordion of
        Accordion c ->
            c

        Log _ c ->
            c


{-| -}
mapTemplates : (Segment.Templates -> Segment.Templates) -> Accordion -> Accordion
mapTemplates fu =
    mapConfig <| \c -> { c | templates = fu c.templates }


{-| -}
exit : Accordion -> Accordion
exit =
    mapTree <|
        \t ->
            if Tree.isRoot t then
                t

            else
                Tree.up t


{-| The focus is on a root node
-}
isRoot : Accordion -> Bool
isRoot =
    config >> .tree >> Tree.isRoot


{-| Go upmost
-}
root : Accordion -> Accordion
root =
    mapTree Tree.root



---- A C T I O N S ----


{-| Serialize the creation and modifications of an `Accordion`
-}
type Action
    = Name String
    | Modify Segment.Action
    | Go Direction
    | Insert Direction
    | Delete
    | Undo IntentId


{-| -}
actionCodec : Codec Action
actionCodec =
    Codec.custom
        (\name modify go_ insert delete_ undo value ->
            case value of
                Name s ->
                    name s

                Modify a ->
                    modify a

                Go d ->
                    go_ d

                Insert d ->
                    insert d

                Delete ->
                    delete_

                Undo i ->
                    undo i
        )
        |> variant1 "Name" Name string
        |> variant1 "Modify" Modify Segment.actionCodec
        |> variant1 "Go" Go Fold.directionCodec
        |> variant1 "Insert" Insert Fold.directionCodec
        |> variant0 "Delete" Delete
        |> variant1 "Undo" Undo intentIdCodec
        |> Codec.buildCustom


type alias History =
    List Intent


historyCodec : Codec History
historyCodec =
    Codec.list intentCodec


type alias Intent =
    { intentId : IntentId, location : Maybe String, action : Action }


intentCodec : Codec Intent
intentCodec =
    Codec.object Intent
        |> Codec.field "intentId" .intentId intentIdCodec
        |> Codec.maybeField "location" .location string
        |> Codec.field "action" .action actionCodec
        |> Codec.buildObject


withoutHistory : Accordion -> Accordion
withoutHistory =
    mapConfig (\c -> { c | tree = Tree.singleton Segment.empty })
        >> mapLog (\l -> { l | history = [] })


{-| Use this function to preserve templates and such
-}
reviseHistory : History -> Accordion -> Accordion
reviseHistory history accordion =
    let
        applyIntent : Intent -> Accordion -> Accordion
        applyIntent { location, action } acc =
            location
                |> Maybe.map
                    (\l ->
                        if l /= focusId acc then
                            goToClosestId l acc

                        else
                            acc
                    )
                |> Maybe.withDefault acc
                |> (case action of
                        Name caption ->
                            Segment.singleton caption
                                |> setSegment

                        Modify segmentAction ->
                            Segment.apply segmentAction
                                |> mapFocus

                        Go direction ->
                            go direction

                        Insert direction ->
                            insertEmpty direction

                        Delete ->
                            delete

                        Undo _ ->
                            identity
                   )
    in
    markUndones history
        |> undoUndones
        |> List.foldl applyIntent (withoutHistory accordion)
        |> mapLog (\l -> { l | history = history })


{-| -}
create : Segment.Templates -> History -> Accordion
create templates history =
    Log { viewingHistory = True, history = history } { tree = Tree.singleton Segment.empty, templates = templates }
        |> reviseHistory history


type alias IntentId =
    { sessionId : String, ordinal : Int }


intentIdCodec : Codec IntentId
intentIdCodec =
    Codec.object IntentId
        |> Codec.field "sessionId" .sessionId string
        |> Codec.field "ordinal" .ordinal Codec.int
        |> Codec.buildObject


{-| Decides, for each action, if it is undone or not:

  - If an Action is undone multiple times, it remains undone
  - If the Undo is undone, the action it undid is reinstated
  - If the Undo of an Undo is undone, the action is re-undone, etc.

-}
markUndones : History -> List ( { isUndone : Bool, by : IntentId }, Intent )
markUndones history =
    let
        -- (undone, who must be undone to undo/redo)
        whoUndid : IntentId -> { isUndone : Bool, by : IntentId }
        whoUndid key =
            history
                |> List.filterMap
                    (\intent ->
                        case intent.action of
                            Undo targetId ->
                                Just { origin = intent.intentId, target = targetId }

                            _ ->
                                Nothing
                    )
                |> List.find (.target >> (==) key)
                |> Maybe.map
                    (.origin
                        >> whoUndid
                        >> (\rootCause ->
                                { rootCause | isUndone = not rootCause.isUndone }
                           )
                    )
                |> Maybe.withDefault { isUndone = False, by = key }
    in
    history
        |> List.map (\intent -> ( whoUndid intent.intentId, intent ))


undoUndones : List ( { isUndone : Bool, by : IntentId }, Intent ) -> List Intent
undoUndones =
    List.filterMap <|
        \( { isUndone }, intent ) ->
            if isUndone then
                Nothing

            else
                Just intent



---- Navigate -----


{-| finds closest string match via Levinshtain distance;
if "", go to root, if is leaf, remain on leaf, else go to direct descendant focus
-}
goToParentId : String -> Accordion -> Accordion
goToParentId pId =
    case pId of
        "" ->
            mapTree Tree.root

        _ ->
            goToClosestId pId >> mapTree (Fail identity |> Walk Down |> Tree.go)


{-| -}
goToId : String -> Accordion -> Accordion
goToId id =
    .id >> (==) id |> Tree.Find |> Tree.go |> mapTree


goToClosestId : String -> Accordion -> Accordion
goToClosestId id acc =
    mapTree (.id >> (==) (closestId id acc) |> Tree.Find |> Tree.go) acc


go : Direction -> Accordion -> Accordion
go direction =
    Branch.singleton Segment.empty |> Tree.Insert |> Walk direction |> Tree.go |> mapTree



---- Map ----


{-| -}
mapTree : (Tree Segment -> Tree Segment) -> Accordion -> Accordion
mapTree fu =
    mapConfig <| \c -> { c | tree = fu c.tree }


insertEmpty : Direction -> Accordion -> Accordion
insertEmpty direction =
    (Tree.insert direction Segment.empty |> mapTree) >> go direction >> setSegment Segment.empty


delete : Accordion -> Accordion
delete =
    mapTree Tree.deleteIfPossible


{-| The segment.id is made unique by appending an incrementing suffix if necessary
-}
setSegment : Segment -> Accordion -> Accordion
setSegment segment accordion =
    let
        id =
            segment.id

        { tree } =
            config accordion

        autoSuffix : Int -> String
        autoSuffix int =
            let
                testId =
                    id ++ "-(" ++ String.fromInt int ++ ")"
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
    mapFocus (\_ -> { segment | id = uniqueId }) accordion


mapFocus : (Segment -> Segment) -> Accordion -> Accordion
mapFocus =
    Tree.mapFocus >> mapTree



---- Query ----


{-| can be used to generate links, for example in Toc or Search;
defaults to ""
-}
closestId : String -> Accordion -> String
closestId searchString =
    config
        >> .tree
        >> Tree.flatten
        >> List.minimumBy (.id >> Levenshtein.distance searchString)
        >> Maybe.map .id
        >> Maybe.withDefault ""



---- Decompose ----


{-| defaults to "" if root
-}
parentId : Accordion -> String
parentId =
    config
        >> (\{ tree } ->
                if Tree.isRoot tree then
                    ""

                else
                    Tree.up tree |> Tree.focus |> .id
           )


{-| -}
focusId : Accordion -> String
focusId =
    config >> .tree >> Tree.focus >> .id



---- Update ----


{-| -}
type Msg
    = TemplatesUpdated (Segment.Templates -> Segment.Templates)
    | LogOpened
    | LogClosed


{-| -}
update : Msg -> Accordion -> Accordion
update msg =
    case msg of
        TemplatesUpdated fu ->
            mapConfig <| \c -> { c | templates = fu c.templates }

        LogOpened ->
            mapLog <| \l -> { l | viewingHistory = True }

        LogClosed ->
            mapLog <| \l -> { l | viewingHistory = False }



---- View ----


type Renderable msg
    = Item (Ui msg)
    | Var ( String, Int )
    | Class String


type alias ViewMode msg =
    { zone : Maybe ( String, Time.Zone )
    , do : (String -> Intent) -> msg
    , volatile : Msg -> msg
    }


{-| -}
view :
    ViewMode msg
    -> Accordion
    -> Ui msg
view { zone, do, volatile } accordion =
    let
        c =
            config accordion

        {- given a history and the id of the current session, the Accordion can generate an `Intent` from location and action. -}
        generateIntent : String -> Action -> String -> Intent
        generateIntent location action sessionId =
            let
                history =
                    case accordion of
                        Accordion _ ->
                            []

                        Log l _ ->
                            l.history

                latestOrdinal =
                    history
                        |> List.filter (.intentId >> .sessionId >> (==) sessionId)
                        |> List.map (.intentId >> .ordinal)
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            { intentId = { sessionId = sessionId, ordinal = latestOrdinal + 1 }, location = Just location, action = action }

        atFocus : Action -> msg
        atFocus =
            generateIntent (focusId accordion) >> do

        viewLog : Html msg
        viewLog =
            let
                intentIdToString : IntentId -> String
                intentIdToString iid =
                    iid.sessionId ++ ":" ++ String.fromInt iid.ordinal
            in
            case accordion of
                Log { history, viewingHistory } _ ->
                    if viewingHistory then
                        markUndones history
                            |> List.indexedMap
                                (\_ ( { isUndone, by }, intent ) ->
                                    Html.li []
                                        [ Html.pre [] [ Maybe.map ((++) "at " >> Html.text) intent.location |> Maybe.withDefault (Html.text "*") ]
                                        , Ui.singlePickOrNot isUndone ( intentIdToString intent.intentId, atFocus (Undo by) )
                                        , "undone by " ++ intentIdToString by |> Html.text |> Ui.notIf (not isUndone)
                                        , Html.pre [] [ Html.text (encoder actionCodec intent.action |> Encode.encode 4) ]
                                        ]
                                )
                            |> Html.ol [ class "list" ]
                            |> (\l ->
                                    Html.fieldset [ id "activityLog" ]
                                        [ Html.legend []
                                            [ Html.span [] [ Html.text ("Activity Log (" ++ String.fromInt (List.length history) ++ ")") ]
                                            , Html.button [ onClick (volatile LogClosed) ] [ Html.text "close" ]
                                            ]
                                        , Html.p [] [ Html.text "You can undo and redo any previous action in this list" ]
                                        , l
                                        ]
                               )

                    else
                        Html.button [ id "activityLog", onClick (volatile LogOpened) ] [ Html.text "Activity Log..." ]

                Accordion _ ->
                    Ui.none

        viewSegment =
            Segment.edit { zone = zone, do = \location -> Modify >> generateIntent location >> do, insert = Insert >> atFocus, delete = Delete |> atFocus, templates = c.templates, updateTemplates = TemplatesUpdated >> volatile, context = Tree.split c.tree }

        classes : Html.Attribute msg
        classes =
            classList
                [ ( "\u{1FA97}" ++ Segment.orientationToString (Segment.orientation (Tree.focus c.tree)), True )
                , ( "aisleHasBody", List.any (Segment.hasBody c) (Tree.getAisleNodes c.tree |> Zipper.flat) )
                , ( "focusHasBody", Segment.hasBody c (Tree.focus c.tree) )
                , ( "focusIsRoot", Tree.isRoot c.tree )
                , ( "focusIsBackground", Segment.isBackground (Tree.focus c.tree) )
                ]

        findPeekConfig : Segment -> { targetId : String, hint : String }
        findPeekConfig seg =
            let
                peekParent =
                    .id
                        >> (==) seg.id
                        |> Tree.Find
                        |> Tree.go
                        |> (|>) c.tree
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
                                    if Segment.isIllustration c seg then
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
                    ( ViewSegment.addWidth mode (Segment.hasBody c segment) (Segment.width segment) (Segment.infoLineCount segment) offset
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
            , ( "logMenu", viewLog )
            ]

        renderAccordion : List (Renderable msg) -> Ui msg
        renderAccordion =
            List.foldl
                (\renderable ->
                    case renderable of
                        Item ui ->
                            Tuple.mapFirst ((::) ui)

                        Var va ->
                            Tuple.mapSecond ((::) (css [ Layout.toProperty va ]))

                        Class cl ->
                            Tuple.mapSecond ((::) (class cl))
                )
                ( [], [] )
                >> (\( items, attrs ) ->
                        Ui.composeScenes
                            (\scenes ->
                                ( "Accordion"
                                , Keyed.ul
                                    (class "Accordion" :: classes :: attrs)
                                    (List.sortBy Tuple.first overlays ++ scenes)
                                )
                            )
                            (Ui.concat items)
                   )
    in
    c.tree
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
