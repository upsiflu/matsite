module Accordion exposing
    ( Accordion
    , History, historyCodec
    , Intent, intentCodec
    , create
    , exit, reviseHistory, mapTemplates
    , Msg, update
    , Action(..), actionCodec
    , goToId, goToParentId
    , parentId, focusId
    , isRoot
    , closestId
    , view
    , directory
    )

{-|


## To Do

  - [ ] Check `exit` behavior (mainly pertains to Main)

---

@docs Accordion

@docs History, historyCodec
@docs Intent, intentCodec


# Create

@docs create


# Modify

@docs exit, reviseHistory, mapTemplates


# Update

@docs Msg, update


# Persist

@docs Action, actionCodec


# Navigate

@docs goToId, goToParentId


# Deconstruct

@docs parentId, focusId

@docs isRoot


# Query

@docs closestId


# View

@docs view

-}

import Accordion.Segment as Segment exposing (Region(..), Segment)
import Article as Article exposing (Article, Orientation(..), hasBody)
import Article.Fab as Fab
import Codec exposing (Codec, encoder, int, string, variant0, variant1)
import Css exposing (..)
import Directory exposing (Directory)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html exposing (Html, node)
import Html.Styled.Attributes exposing (attribute, class, classList, css, id)
import Html.Styled.Events as Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Layout
import Levenshtein
import List.Extra as List
import Maybe.Extra as Maybe
import String
import Time
import Ui exposing (Ui)
import Zipper
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Tree as Tree exposing (Edge(..), EdgeOperation(..), Tree, Walk(..))


{-|

  - `tree`: the result of the History, but the `focus` position is volatile and kept even when the server sends a revised History
  - `templates`: volatile switchable presets for editing

-}
type Accordion
    = Accordion Config
    | Log LogConfig Config


type alias Config =
    { tree : Tree Article, templates : Article.Templates, syncing : Bool }


type alias LogConfig =
    { viewingHistory : Bool, editing : Bool, history : History }


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
tree : Accordion -> Tree Article
tree =
    config >> .tree


{-| -}
mapTemplates : (Article.Templates -> Article.Templates) -> Accordion -> Accordion
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



---- A C T I O N S ----


{-| Serialize the creation and modifications of an `Accordion`
-}
type Action
    = Name String
    | Modify Article.Action
    | Go Direction
    | Insert Direction
    | Delete
    | Undo IntentId


{-| -}
actionCodec : Codec Action
actionCodec =
    Codec.custom
        (\name modify go_ insert delete_ undo_ value ->
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
                    undo_ i
        )
        |> variant1 "Name" Name string
        |> variant1 "Modify" Modify Article.actionCodec
        |> variant1 "Go" Go Fold.directionCodec
        |> variant1 "Insert" Insert Fold.directionCodec
        |> variant0 "Delete" Delete
        |> variant1 "Undo" Undo intentIdCodec
        |> Codec.buildCustom


{-| -}
type alias History =
    List Intent


{-| -}
historyCodec : Codec History
historyCodec =
    Codec.list intentCodec


{-| -}
type alias Intent =
    { intentId : IntentId, location : Maybe String, action : Action }


{-| -}
intentCodec : Codec Intent
intentCodec =
    Codec.object Intent
        |> Codec.field "intentId" .intentId intentIdCodec
        |> Codec.maybeField "location" .location string
        |> Codec.field "action" .action actionCodec
        |> Codec.buildObject


withoutHistory : Accordion -> Accordion
withoutHistory =
    mapConfig (\c -> { c | tree = Tree.singleton Article.empty })
        >> mapLog (\l -> { l | history = [] })


{-| Use this function to preserve templates and such
-}
injectHistory : History -> Accordion -> Accordion
injectHistory history accordion =
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
                            Article.singleton caption
                                |> setArticle

                        Modify segmentAction ->
                            Article.apply segmentAction
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
        |> goToId (focusId accordion)


{-| Use this function to preserve templates and such
-}
reviseHistory : History -> Accordion -> Accordion
reviseHistory history accordion =
    if (config accordion).syncing then
        injectHistory history accordion

    else
        accordion


{-| -}
create : Article.Templates -> History -> Accordion
create templates history =
    Log { viewingHistory = False, editing = False, history = history } { tree = Tree.singleton Article.empty, templates = templates, syncing = False }
        |> injectHistory history


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


undo : History -> Maybe Action
undo =
    markUndones
        >> List.reverse
        >> List.find
            (Tuple.second
                >> .action
                >> (\a ->
                        case a of
                            Undo _ ->
                                False

                            _ ->
                                True
                   )
            )
        >> Maybe.andThen
            (\( { isUndone }, intent ) ->
                if isUndone then
                    Nothing

                else
                    Just (Undo intent.intentId)
            )



{- find the last non-Undo action.
   If it is undone, undo its undoer.
-}


redo : History -> Maybe Action
redo =
    markUndones
        >> List.reverse
        >> List.findMap
            (\( { isUndone, by }, intent ) ->
                case ( isUndone, by, intent.action ) of
                    ( _, _, Undo _ ) ->
                        Nothing

                    ( True, undoer, _ ) ->
                        Just (Undo undoer)

                    _ ->
                        Nothing
            )



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
    Branch.singleton Article.empty |> Tree.Insert |> Walk direction |> Tree.go |> mapTree



---- Map ----


{-| -}
mapTree : (Tree Article -> Tree Article) -> Accordion -> Accordion
mapTree fu =
    mapConfig <| \c -> { c | tree = fu c.tree }


insertEmpty : Direction -> Accordion -> Accordion
insertEmpty direction =
    (Tree.insert direction Article.empty |> mapTree) >> go direction >> setArticle Article.empty


delete : Accordion -> Accordion
delete =
    mapTree Tree.deleteIfPossible


{-| The segment.id is made unique by appending an incrementing suffix if necessary
-}
setArticle : Article -> Accordion -> Accordion
setArticle segment accordion =
    let
        id =
            segment.id

        t =
            tree accordion

        autoSuffix : Int -> String
        autoSuffix int =
            let
                testId =
                    id ++ "-(" ++ String.fromInt int ++ ")"
            in
            if Tree.any (.id >> (==) testId) t then
                autoSuffix (int + 1)

            else
                testId

        uniqueId =
            if Tree.any (.id >> (==) id) t then
                autoSuffix 0

            else
                id
    in
    mapFocus (\_ -> { segment | id = uniqueId }) accordion


mapFocus : (Article -> Article) -> Accordion -> Accordion
mapFocus =
    Tree.mapFocus >> mapTree



---- Query ----


{-| can be used to generate links, for example in Toc or Search;
defaults to ""

---

better use Directory!

-}
closestId : String -> Accordion -> String
closestId searchString =
    config
        >> .tree
        >> Tree.flatten
        >> List.minimumBy (.id >> Levenshtein.distance searchString)
        >> Maybe.map .id
        >> Maybe.withDefault ""


directory : Accordion -> Directory
directory =
    tree
        >> Tree.flatMap (.id >> (\id -> ( id, id )))
        >> Directory.fromList



---- Decompose ----


{-| defaults to "" if root
-}
parentId : Accordion -> String
parentId =
    tree
        >> (\t ->
                if Tree.isRoot t then
                    ""

                else
                    Tree.up t |> Tree.focus |> .id
           )


{-| -}
focusId : Accordion -> String
focusId =
    tree >> Tree.focus >> .id



---- Update ----


{-| -}
type Msg
    = TemplatesUpdated (Article.Templates -> Article.Templates)
    | LogToggled
    | EditingToggled
    | SyncingToggled


{-| -}
update : Msg -> Accordion -> Accordion
update msg =
    case msg of
        TemplatesUpdated fu ->
            mapConfig <| \c -> { c | templates = fu c.templates }

        SyncingToggled ->
            mapConfig <| \c -> { c | syncing = not c.syncing }

        LogToggled ->
            mapLog <| \l -> { l | viewingHistory = not l.viewingHistory }

        EditingToggled ->
            mapLog <| \l -> { l | editing = not l.editing }



---- View ----


type Renderable msg
    = Item (Ui msg)
    | Var ( String, Int )
    | Class String


type alias ViewMode msg =
    { zone : ( String, Time.Zone )
    , now : Time.Posix
    , do : (String -> Intent) -> msg
    , volatile : Msg -> msg
    , scrolledTo : String -> msg
    , scrolledIntoNowhere : msg
    }


{-| -}
view :
    ViewMode msg
    -> Accordion
    -> Ui msg
view ({ zone, now, do, scrolledTo, scrolledIntoNowhere, volatile } as mode) accordion =
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

        breadcrumbs : String -> (() -> List Article)
        breadcrumbs sid () =
            goToId sid accordion
                |> config
                |> .tree
                |> Tree.breadcrumbs

        branch : String -> (() -> Branch Article)
        branch sid () =
            goToId sid accordion
                |> config
                |> .tree
                |> Tree.focusedBranch

        viewLog : Html msg
        viewLog =
            let
                intentIdToString : IntentId -> String
                intentIdToString iid =
                    iid.sessionId ++ ":" ++ String.fromInt iid.ordinal
            in
            Html.div [ id "activityLog" ]
                [ case accordion of
                    Log { history, viewingHistory } _ ->
                        if viewingHistory then
                            markUndones history
                                |> List.indexedMap
                                    (\_ ( { isUndone, by }, intent ) ->
                                        Html.li []
                                            [ Html.pre [] [ Maybe.map ((++) "at " >> Html.text) intent.location |> Maybe.withDefault (Html.text "*") ]
                                            , Ui.singlePickOrNot (not isUndone)
                                                (( intentIdToString intent.intentId, atFocus (Undo by) )
                                                    |> (\( str, msg ) ->
                                                            ( { front = [ Html.text str ], title = "Select a Byline if needed" }
                                                            , Just msg
                                                            )
                                                       )
                                                )
                                            , "undone by " ++ intentIdToString by |> Html.text |> Ui.notIf (not isUndone)
                                            , Html.pre [] [ Html.text (encoder actionCodec intent.action |> Encode.encode 4) ]
                                            ]
                                    )
                                |> Html.ol [ class "list" ]
                                |> (\l ->
                                        Html.fieldset []
                                            [ Html.legend []
                                                [ Html.span [] [ Html.text ("Activity Log (" ++ String.fromInt (List.length history) ++ ")") ]
                                                , Html.button [ onClick (volatile LogToggled) ] [ Html.text "close" ]
                                                ]
                                            , Ui.check { front = [ Html.text "Synchronize with Databas" ], title = "If you don't connect to the database, the app uses a minimal preset structure" } (volatile SyncingToggled) (Just c.syncing)
                                            , Html.p [] [ Html.text "You can undo and redo any previous action in this list by clicking on the green buttons." ]
                                            , l
                                            ]
                                   )

                        else
                            Html.section [ class "ui control" ]
                                [ Html.button [ class "ui", onClick (volatile LogToggled) ] [ Html.text "Activity Log..." ] ]

                    Accordion _ ->
                        Ui.none
                ]

        viewArticle : Segment -> Ui msg
        viewArticle =
            (case accordion of
                Log { editing } _ ->
                    if editing then
                        Segment.edit

                    else
                        Segment.view

                _ ->
                    Segment.view
            )
                { zone = zone
                , now = now
                , directory = directory accordion
                , templates = c.templates
                , do = \location -> Modify >> generateIntent location >> do
                , delete = Delete |> atFocus
                , rename = Name >> atFocus
                , insert = Insert >> atFocus
                }

        editAccordion sheets =
            case accordion of
                Log { history, editing } _ ->
                    if editing then
                        sheets
                            ++ [ Html.div [ class "stretch-h" ]
                                    [ Ui.squareToggleButton { front = [ Html.span [] [ Html.text "↶" ] ], title = "Undo" } False (undo history |> Maybe.map atFocus)
                                    , Ui.toggleModeButton { front = [ Html.span [] [ Html.text "Done" ] ], title = "Browse the page as if you were a visitor" } True (Just (volatile EditingToggled))
                                    , Ui.squareToggleButton { front = [ Html.span [] [ Html.text "↷" ] ], title = "Redo" } False (redo history |> Maybe.map atFocus)
                                    ]
                               ]

                    else
                        [ Html.div [ class "stretch-h" ]
                            [ Ui.toggleModeButton { front = [ Html.span [] [ Html.text "Edit" ] ], title = "Change properties of the Articles" } False (Just (volatile EditingToggled))
                            ]
                            |> Ui.notIf True
                        ]

                _ ->
                    [ Html.div [ class "stretch-h" ]
                        [ Html.text ""
                        ]
                    ]

        statistics : Html.Attribute msg
        statistics =
            classList
                [ ( "\u{1FA97}" ++ Article.orientationToString (Article.orientation (Tree.focus c.tree)), True )
                , ( "aisleHasBody", List.any (Article.hasBody c) (Tree.getAisleNodes c.tree |> Zipper.flat) )
                , ( "focusHasBody", Article.hasBody c (Tree.focus c.tree) )
                , ( "focusIsRoot", Tree.isRoot c.tree )
                , ( "focusIsBackground", Article.isBackground (Tree.focus c.tree) )
                ]

        createRegions : C -> List ( Region, List A )
        createRegions { up, left, x, here, nest, y, right, down } =
            let
                focusedArticle =
                    Tree.focus c.tree

                focusedBranch =
                    Tree.focusedBranch c.tree

                ---- PEEK ----
                maybePeekTargetBranch : Maybe (Branch Article)
                maybePeekTargetBranch =
                    if hasBody c focusedArticle then
                        Nothing

                    else
                        focusedBranch
                            ---- 1. UPCOMING
                            |> Branch.subBranches
                            |> List.filter
                                (Branch.node >> .fab >> Maybe.map (Fab.isUpcoming mode) >> Maybe.withDefault False)
                            |> List.minimumBy
                                (Branch.node >> .fab >> Maybe.andThen (Fab.nextBeginning mode) >> Maybe.withDefault 2147483646)
                            ---- 2. DIRECT CHILD
                            |> Maybe.orElse (Just focusedBranch)

                -- Find the closest illustration, among the nest, for a given branch of segments
                peekTargetBranchToIllustration : Branch Article -> Maybe Article
                peekTargetBranchToIllustration =
                    Branch.flat
                        >> List.filter (\a -> List.member a (List.map Tuple.second nest))
                        >> List.find (Article.isIllustration c)

                ( peekConfig, maybePeek, cache ) =
                    case maybePeekTargetBranch of
                        Just targetBranch ->
                            let
                                illu =
                                    peekTargetBranchToIllustration targetBranch

                                cche =
                                    Maybe.map
                                        (\ill -> List.filter (Tuple.second >> (/=) ill) nest)
                                        illu
                                        |> Maybe.withDefault nest

                                head =
                                    Branch.node targetBranch
                            in
                            ( { targetId = head.id, hint = Article.hint zone head }
                            , illu |> Maybe.map (Tuple.pair Fold.fataMorganaPosition)
                            , cche
                            )

                        Nothing ->
                            ( Segment.defaultPeekConfig
                            , Just ( Fold.fataMorganaPosition, Article.defaultIllustration )
                            , nest
                            )
            in
            [ ( North, List.reverse up )
            , ( West, List.reverse left )
            , ( Center, [ here ] )
            , ( Peek peekConfig, Maybe.toList maybePeek )
            , ( Cache, cache )
            , ( East, right )
            , ( South, down )
            ]
                ++ (case here |> Tuple.second |> Article.orientation of
                        Horizontal ->
                            [ ( NearWest, List.reverse x )
                            , ( NearEast, y )
                            , ( NearNorth, [] )
                            , ( NearSouth, [] )
                            ]

                        Vertical ->
                            [ ( NearNorth, List.reverse x )
                            , ( NearSouth, y )
                            , ( NearWest, [] )
                            , ( NearEast, [] )
                            ]
                   )

        renderRegion : ( Region, List A ) -> List (Renderable msg)
        renderRegion ( region, list ) =
            list
                |> List.foldl
                    (\( position, article ) ( offset, newList ) ->
                        let
                            segment =
                                { ---- Position
                                  position = position
                                , region = region
                                , offset = offset

                                ---- Data
                                , article = article

                                ---- Lazy Neighbors
                                , breadcrumbs = breadcrumbs article.id
                                , branch = branch article.id
                                }
                        in
                        ( Segment.addWidth c segment (Article.hasBody c article) (Article.width article) offset
                        , viewArticle segment :: newList
                        )
                    )
                    ( Segment.zeroOffset, [] )
                |> (\( totalOffset, renderedArticles ) ->
                        Segment.offsetToCssVariables totalOffset
                            |> List.map (Tuple.mapFirst ((++) (Segment.regionToString region ++ "-")) >> Var)
                            |> (++) (List.map Item renderedArticles)
                   )

        overlays : List ( String, Html msg )
        overlays =
            [ ( "screenBackground", Html.div [ class "screenBackground" ] [] )
            , ( "aisleBackground", Html.div [ class "aisleBackground" ] [] )
            , ( "xy", Html.div [ id "xy" ] [] )

            --the following will be sorted to be the first element so it can influence the others via css ~
            , ( " "
              , node "closest-aisle"
                    [ attribute "increment" (focusId accordion)
                    , id "virtualScreen"
                    , Decode.at [ "detail" ] Decode.string
                        |> Decode.map scrolledTo
                        |> Events.on "scrolledToA"
                    , Events.on "scrolledIntoNowhere" (Decode.succeed scrolledIntoNowhere)
                    ]
                    []
              )
            ]
                ++ (if False then
                        [ ( "logMenu", viewLog ) ]

                    else
                        []
                   )

        globalToolbar : Ui msg
        globalToolbar =
            Ui.fromEmpty
                (\ui ->
                    { ui
                        | handle =
                            Layout.hamburgerMenu <|
                                if isRoot accordion then
                                    Nothing

                                else
                                    Just "/"
                    }
                )

        propertySheet : Ui msg
        propertySheet =
            Ui.fromEmpty <|
                \e ->
                    { e
                        | control =
                            Ui.check { front = [ Html.label [] [ Html.text "Show presets" ] ], title = "This option lets you copy from preset content. Turn it off and paste into live segments." }
                                (volatile (TemplatesUpdated Article.toggleTemplates))
                                (Article.templatesAreOn (accordion |> config |> .templates))
                    }

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
                        Ui.concat (propertySheet :: items)
                            |> Ui.composeScenes
                                (\scenes ->
                                    ( "Accordion"
                                    , Keyed.ul
                                        (class "Accordion" :: statistics :: attrs)
                                        (List.sortBy Tuple.first (overlays ++ scenes))
                                    )
                                )
                            |> Ui.composeControls
                                (editAccordion
                                    >> Html.section [ class "ui sheet" ]
                                )
                            |> Ui.with globalToolbar
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
    ( Position, Article )


{-| Branch accumulation
-}
type alias B =
    { orientation : Orientation, role : Role, here : A, nest : List A, left : List A, right : List A, down : List A }


{-| Tree accumulation
-}
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
            { orientation = Article.orientation segment, role = position.role, here = here, nest = [], left = [], right = [], down = [] }
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


{-| This fold sorts the segments, beginning at the focus, into `C`, comprising four direction, `here`, and a catchall `nest`
-}
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
                case Article.orientation segment of
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
