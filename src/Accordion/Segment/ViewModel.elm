module Accordion.Segment.ViewModel exposing
    ( Region(..)
    , regionToString
    , addWidth, Offset, cumulativeOffset
    , offsetToCssVariables, zeroOffset
    , path
    , toCssVariables
    , toClass
    , ViewModel, defaultPeekConfig, edit, fab, occ, view
    )

{-| reflects a Segment's momentary position within the Tree
and provides View-related functions

![Accordion Structure](../asset/22-03-17-Accordion.svg)


# Create

@docs defaultPeek

---

@docs ViewMode

---

@docs Region
@docs regionToString


### Width

@docs Width, addWidth, widthToString, Offset, cumulativeOffset
@docs offsetToCssVariables, zeroOffset


# Deconstruct

@docs path
@docs toCssVariables


# View

@docs toClass

-}

import Accordion.Segment as Segment exposing (Action(..), BodyChoice(..), BodyTemplate(..), InfoChoice(..), InfoTemplate(..), Segment, Templates, Width(..), bodyTypeToString, getTemplate, infoTypeToString)
import Accordion.Segment.Fab as Fab exposing (Fab)
import Bool.Extra exposing (ifElse)
import Css exposing (..)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed as Keyed
import Layout
import List.Extra as List
import Maybe.Extra as Maybe
import Occurrence exposing (Occurrence)
import Time
import Ui exposing (Ui)
import Zipper
import Zipper.Branch as Branch exposing (Branch)


{-|

    The ViewModel encodes a Segment's momentary state for viewing:
    - logical position (in relation to the enclosing Structure)
    - visual position (in relation to the screen)
    - persistent data
    - neighbors (lazy)

-}
type alias ViewModel =
    { ---- Position
      position : Position
    , region : Region
    , offset : Offset

    ---- Data
    , segment : Segment

    ---- Lazy Neighbors
    , breadcrumbs : () -> List Segment
    , branch : () -> Branch Segment
    }


{-| -}
type Region
    = North
    | South
    | West
    | East
    | NearWest
    | NearEast
    | Center
    | Peek { targetId : String, hint : String }
    | Cache


{-| -}
defaultPeekConfig : { targetId : String, hint : String }
defaultPeekConfig =
    { targetId = "", hint = "" }


{-| -}
path : ViewModel -> List Direction
path =
    .position >> .path



---- View ----


{-| -}
regionToString : Region -> String
regionToString region =
    case region of
        North ->
            "north"

        South ->
            "south"

        West ->
            "west"

        East ->
            "east"

        NearWest ->
            "nearWest"

        NearEast ->
            "nearEast"

        Center ->
            "center"

        Peek _ ->
            "peek"

        Cache ->
            "cache"


{-| encode the cumulative shift from the origin in a series of Segments
-}
type alias Offset =
    { screens : Int, columns : Int, units : Int, headers : Int, infoLines : Int }


{-| -}
zeroOffset : Offset
zeroOffset =
    { screens = 0, columns = 0, units = 0, headers = 0, infoLines = 0 }


{-| -}
cumulativeOffset : List { m | offset : Offset } -> Offset
cumulativeOffset =
    let
        addOffset : Offset -> Offset -> Offset
        addOffset { screens, columns, units, headers, infoLines } acc =
            { screens = screens + acc.screens, columns = columns + acc.columns, units = units + acc.units, headers = headers + acc.headers, infoLines = infoLines + acc.infoLines }
    in
    zeroOffset
        |> List.foldl (.offset >> addOffset)


isPeek : ViewModel -> Bool
isPeek { region } =
    case region of
        Peek _ ->
            True

        _ ->
            False


isParent : ViewModel -> Bool
isParent =
    path >> (==) [ Up ]


{-| -}
addWidth : { c | templates : Segment.Templates } -> ViewModel -> Bool -> Width -> Offset -> Offset
addWidth config model isExpanded width acc =
    let
        respectInfoLines : Bool
        respectInfoLines =
            model.region == Center || isPeek model || isParent model

        respectedInfoLines =
            if respectInfoLines then
                byline config model |> Tuple.second

            else
                0
    in
    case ( isExpanded, width ) of
        ( True, Columns c ) ->
            { acc | columns = acc.columns + c, units = acc.units + 1 + respectedInfoLines, infoLines = acc.infoLines + respectedInfoLines }

        ( True, Screen ) ->
            { acc | screens = acc.screens + 1, units = acc.units + 1 + respectedInfoLines, infoLines = acc.infoLines + respectedInfoLines }

        ( False, _ ) ->
            { acc | headers = acc.headers + 1, units = acc.units + 1 + respectedInfoLines, infoLines = acc.infoLines + respectedInfoLines }


toString : ViewModel -> String
toString mode =
    let
        pos =
            Fold.positionToString mode.position

        reg =
            regionToString mode.region

        off =
            mode.offset
                |> (\o ->
                        "🏛️" ++ String.fromInt o.columns ++ " 💻" ++ String.fromInt o.screens ++ " 🟡" ++ String.fromInt o.units
                   )
    in
    [ pos, reg, off ]
        |> String.join " "


{-| -}
toClass : ViewModel -> Html.Attribute msg
toClass =
    toString >> class


{-| -}
toCssVariables : ViewModel -> Html.Attribute msg
toCssVariables =
    .offset >> offsetToCssVariables >> List.map Layout.toProperty >> css


{-| -}
offsetToCssVariables : Offset -> List ( String, Int )
offsetToCssVariables { screens, columns, units, headers, infoLines } =
    [ ( "screens", screens )
    , ( "columns", columns )
    , ( "units", units )
    , ( "headers", headers )
    , ( "infoLines", infoLines )
    ]



---- Helper function to extract Contextual contents ----


{-|

1.  OWN: The segment's own occurrence
2.  COMBINED: The merged occurrences of itself and all its offspring
3.  INHERITED: The first occurrence encountered upward its ancestors

-}
occ : ViewModel -> Maybe Occurrence
occ { segment, branch, breadcrumbs } =
    segment.fab
        |> Maybe.andThen Fab.occurrence
        |> Maybe.orElseLazy
            (\() ->
                Branch.flat (branch ())
                    |> List.filterMap (.fab >> Maybe.andThen Fab.occurrence)
                    |> List.foldl1 Occurrence.merge
            )
        |> Maybe.orElseLazy
            (\() -> List.findMap (.fab >> Maybe.andThen Fab.occurrence) (breadcrumbs ()))


{-| Note that a Fab is at most calculated once, namely for the `Parent` position which is singular in an Accordion.
This satisfies the Ui rule that an app's screen may at most show one Fab.

    1. F: The .P segment's child's Fab
    2. UPCOMING: The Fab within the offspring with has the earliest upcoming occasion
    3. INHERITED: Any upcoming

-}
fab : { c | now : Time.Posix } -> ViewModel -> Maybe Fab
fab config { position, branch, breadcrumbs } =
    if position.role == Parent then
        branch ()
            |> Branch.focusedChild
            |> Maybe.andThen .fab
            |> Maybe.orElseLazy
                (\() ->
                    Branch.flat (branch ())
                        |> List.filterMap (.fab >> Fab.andUpcoming config)
                        |> List.minimumBy (Fab.nextBeginning config >> Maybe.withDefault 2147483646)
                )
            |> Maybe.orElseLazy
                (\() ->
                    breadcrumbs ()
                        |> List.filterMap (.fab >> Fab.andUpcoming config)
                        |> List.head
                )

    else
        Nothing


{-| Note that, like the Fab, a Toc is only calculated once.

In a future version of the algorithm, we my want to produce a details/summary thing with the summary being the focused list item.
For now, we have a suboptimal solution.

-}
toc : { c | templates : Segment.Templates } -> ViewModel -> Maybe ( Html Never, Int )
toc config { branch, position } =
    if position.role == Parent then
        branch ()
            |> Branch.nextGeneration
            |> Maybe.map
                (Zipper.map Branch.node)
            |> Maybe.map
                (\({ focus } as zipper) ->
                    Zipper.map
                        (\segment ->
                            heading config segment
                                |> Maybe.map
                                    (\entry ->
                                        Html.li
                                            [ classList [ ( "focused", String.contains segment.id focus.id ) ] ]
                                            [ Html.a [ href ("/" ++ segment.id) ] [ Html.text entry ] ]
                                    )
                        )
                        zipper
                        |> Zipper.flat
                        |> List.filterMap identity
                        |> (\l -> ( Html.ul [ class "info toc" ] l, List.length l // 6 ))
                )

    else
        Nothing


{-| -}
heading : { c | templates : Templates } -> Segment -> Maybe String
heading { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just (Content (Just str) _) ) ->
            Just str

        ( CustomContent (Just str), Nothing ) ->
            Just str

        _ ->
            Nothing



---- View ----


byline : { a | templates : Templates } -> ViewModel -> ( Html Never, Int )
byline ({ templates } as config) model =
    case ( getTemplate .info model.segment templates, model.segment.info, model.position.role ) of
        ( Just Toc, _, Parent ) ->
            toc config model
                |> Maybe.withDefault ( Html.text "", 0 )

        ( Just (Byline l b), _, Parent ) ->
            ( b, l )

        ( Nothing, Just CustomToc, Parent ) ->
            toc config model
                |> Maybe.withDefault ( Html.text "", 0 )

        ( Nothing, Just (CustomByline _), Parent ) ->
            ( Html.text "Todo: Load Custom Byline from Database", 1 )

        _ ->
            ( Html.text "", 0 )


{-| In contrast to `view`, we can persist Segment Actions as well as insertions into the Accordion when editing
-}
edit :
    { zone : ( String, Time.Zone )
    , now : Time.Posix
    , templates : Templates
    , do : String -> Action -> msg
    , delete : msg
    , rename : String -> msg
    , insert : Direction -> msg
    }
    -> ViewModel
    -> Ui msg
edit { zone, now, templates, do, delete, rename, insert } ({ position, segment } as model) =
    let
        intend =
            do segment.id

        ( overlay, propertySheet ) =
            let
                overlaidButton dir hint_ symbol =
                    button [ onClick (insert dir), title hint_ ] [ span [] [ text symbol ] ]

                overlaidDeleteButton =
                    details [ class "deleteSegment fly-orientation" ]
                        [ summary [] [ Html.span [] [ Html.text "" ] ]
                        , div [ class "ui flying right-aligned bottom-aligned" ]
                            [ Ui.toggleButton { front = [ span [] [ text "Cut" ] ], title = "Cut this segment" } False Nothing
                            , Ui.toggleButton { front = [ span [] [ text "Copy" ] ], title = "Copy this segment, excluding its id" } False Nothing
                            , Ui.toggleButton { front = [ span [] [ text "Paste" ] ], title = "Paste this segment, excluding its id" } False Nothing
                            , Ui.toggleButton { front = [ span [] [ text "Delete" ] ], title = "Delete this segment" } False (Just delete)
                            ]
                        ]

                template =
                    { body = getTemplate .body segment templates
                    , info = getTemplate .info segment templates
                    }

                maybeNot whatNot whatThen =
                    case whatNot of
                        Nothing ->
                            Just whatThen

                        _ ->
                            Nothing
            in
            case position.role of
                Focus ->
                    let
                        activeOption =
                            case ( template.body, segment.body ) of
                                ( Just _, _ ) ->
                                    "Preset"

                                ( _, custom ) ->
                                    bodyTypeToString custom

                        options =
                            [ ( bodyTypeToString CustomIllustration, intend (WithBody CustomIllustration) )
                            , ( bodyTypeToString (CustomContent Nothing), intend (WithBody (CustomContent Nothing)) )
                            , ( bodyTypeToString (CustomContent (Just "Heading")), intend (WithBody (CustomContent (Just "Heading"))) )
                            ]
                                |> Zipper.create
                                    ( bodyTypeToString PeekThrough, intend (WithBody PeekThrough) )
                                    []
                                |> Zipper.findClosest (Tuple.first >> (==) activeOption)
                                |> Zipper.map
                                    (\( str, msg ) ->
                                        ( { front = [ Html.text str ], title = "Select a Body type for this Segment" }
                                        , maybeNot template.body msg
                                        )
                                    )
                    in
                    ( [ Ui.overlay Ui.Top [ overlaidButton Up "insert empty segment to the top" "+" ]
                      , Ui.overlay Ui.Right [ overlaidButton Right "insert empty segment to the right" "+" ]
                      , Ui.overlay Ui.Bottom [ overlaidButton Down "insert empty segment to the bottom" "+" ]
                      , Ui.overlay Ui.Left [ overlaidButton Left "insert empty segment to the left" "+" ]
                      ]
                    , Html.fieldset [ class "ui" ]
                        [ Html.legend [ class "fill-h" ]
                            [ Ui.textInput "The Unique ID of this segment" segment.id (Just <| \newName -> rename newName), Ui.distanceHolder, overlaidDeleteButton ]
                        , Ui.pick options
                        ]
                    )

                Parent ->
                    let
                        activeOption =
                            case ( template.info, segment.info ) of
                                ( Just _, _ ) ->
                                    "Preset"

                                ( _, custom ) ->
                                    infoTypeToString custom

                        options =
                            [ ( infoTypeToString (Just (CustomByline 1)), intend (WithInfo <| Just (CustomByline 1)) )
                            , ( infoTypeToString (Just (CustomByline 2)), intend (WithInfo <| Just (CustomByline 2)) )
                            ]
                                |> Zipper.create
                                    ( infoTypeToString Nothing, intend (WithInfo Nothing) )
                                    [ ( infoTypeToString (Just CustomToc), intend (WithInfo (Just CustomToc)) ) ]
                                |> Zipper.findClosest (Tuple.first >> (==) activeOption)
                                |> Zipper.map
                                    (\( str, msg ) ->
                                        ( { front = [ Html.text str ], title = "Select a Byline if needed" }
                                        , maybeNot template.info msg
                                        )
                                    )

                        originalCaption =
                            model.segment.caption
                    in
                    ( []
                    , Html.fieldset [ class "ui" ]
                        [ Html.legend [ class "editCaption" ]
                            [ Ui.textInput "Caption" segment.caption.text (Just <| \txt -> intend (WithCaption { originalCaption | text = txt }))
                            , Ui.singlePickOrNot segment.caption.showsDate
                                ( { front = [ Html.text "📅" ], title = "Should the Caption include a date (range)?" }
                                , Just (intend (WithCaption { originalCaption | showsDate = not model.segment.caption.showsDate }))
                                )
                            ]
                        , Ui.pick options
                        , Fab.edit { zone = zone, save = WithFab >> intend } model.segment.fab
                        ]
                    )

                _ ->
                    ( [], Ui.none )

        ui =
            Ui.fromEmpty <| \e -> { e | control = propertySheet }
    in
    view_ { zone = zone, templates = templates, now = now } ui overlay model


{-| -}
view :
    { c | zone : ( String, Time.Zone ), templates : Templates, now : Time.Posix }
    -> ViewModel
    -> Ui msg
view config =
    view_ config (Ui.fromEmpty identity) []


view_ :
    { c | templates : Templates, zone : ( String, Time.Zone ), now : Time.Posix }
    -> Ui msg
    -> List (Html msg)
    -> ViewModel
    -> Ui msg
view_ ({ zone, templates } as config) ui overlays model =
    let
        hideBecauseVeryFarAway =
            model.position.role == Aisle && List.length model.position.path > 5

        viewFab =
            fab config model
                |> Maybe.map (Fab.view config)
                |> Maybe.withDefault Ui.none

        template =
            { body = getTemplate .body model.segment templates
            , info = getTemplate .info model.segment templates
            }

        viewCaption cc =
            let
                oneEntry =
                    Layout.header "" model.segment.id cc.text

                secondEntry o =
                    Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]
                        [ oneEntry
                        , Occurrence.view (Occurrence.Short (Tuple.second zone) Occurrence.Days) o
                            |> Layout.htmlHeader "" model.segment.id
                        ]
            in
            if cc.showsDate then
                model.segment.fab
                    |> Maybe.andThen Fab.occurrence
                    |> Maybe.map secondEntry
                    |> Maybe.withDefault oneEntry

            else
                oneEntry

        viewPeekLink =
            let
                previewOccurrences =
                    occ model
                        |> Maybe.map (Occurrence.view (Occurrence.AsList (Tuple.second zone) Occurrence.Days))
                        |> Maybe.withDefault Ui.none
            in
            case model.region of
                Peek c ->
                    Html.a [ class "peekLink", href c.targetId, title c.hint ]
                        [ Html.h2 [] [ Html.text c.hint ]
                        , previewOccurrences
                        , previewOccurrences
                        ]

                _ ->
                    Ui.none

        viewBody body =
            let
                bodyIsVisible =
                    Segment.isIllustration { templates = templates } model.segment
                        || model.position.role
                        == Focus
                        || model.position.role
                        == Parent
                        || model.position.role
                        == Breadcrumb
                        || model.position.role
                        == Aisle
            in
            (if bodyIsVisible then
                [ Tuple.pair "heading" <|
                    case heading config model.segment of
                        Just h ->
                            Html.a [ href (Layout.sanitise model.segment.id) ] [ Html.h2 [ class "segment-heading" ] [ Html.text h ] ]

                        Nothing ->
                            Html.text ""
                , Tuple.pair "content" <|
                    case ( template.body, body ) of
                        ( Just (Content _ c), _ ) ->
                            c

                        ( Just (Illustration i), _ ) ->
                            i

                        ( Nothing, PeekThrough ) ->
                            Html.text ""

                        ( Nothing, CustomIllustration ) ->
                            Html.node "sync-hypertext" [ attribute "state" "editing", attribute "data-id" ("_" ++ model.segment.id) ] []

                        ( Nothing, CustomContent _ ) ->
                            Html.node "sync-hypertext" [ attribute "state" "editing", attribute "data-id" ("_" ++ model.segment.id) ] []
                ]

             else
                []
            )
                |> Keyed.node "div" [ class "body", classList [ ( "illustrative", Segment.isIllustration config model.segment ) ] ]

        ( viewByline, bylineHeight ) =
            byline config model

        additionalAttributes =
            model.segment.additionalClasses
                |> List.map class

        ownWidthAsVars =
            (case Segment.width model.segment of
                Columns c ->
                    ( c, 0 )

                Screen ->
                    ( 0, 1 )
            )
                |> (\( col, scr ) ->
                        List.map Layout.toProperty
                            [ ( "ownColumns"
                              , col
                              )
                            , ( "ownScreens"
                              , scr
                              )
                            , ( "ownHeaders"
                              , Segment.hasBody config model.segment |> ifElse 1 0
                              )
                            , ( "ownInfoLines"
                              , bylineHeight
                              )
                            ]
                   )
    in
    if hideBecauseVeryFarAway then
        List.indexedMap (\i -> Html.map never >> Tuple.pair (String.fromInt i))
            [ model.segment.caption |> viewCaption
            , Html.div [ class "body waiting" ] []
            ]
            |> Keyed.node "li"
                (id model.segment.id
                    :: toClass model
                    :: class (Segment.orientationToString (Segment.orientation model.segment))
                    :: class (bodyTypeToString model.segment.body)
                    :: Segment.structureClass config model.segment
                    :: toCssVariables model
                    :: css ownWidthAsVars
                    :: additionalAttributes
                )
            |> Tuple.pair model.segment.id
            |> (\scene -> Ui.fromEmpty (\e -> { e | scene = scene }))

    else
        List.map (Html.map never)
            [ model.segment.caption |> viewCaption |> Ui.notIf (Segment.hasBody config model.segment && model.position.isLeaf && not model.position.isRoot) |> Ui.notIf (isPeek model)
            , model.segment.body |> viewBody
            , viewPeekLink
            , viewByline
            , Segment.orientation model.segment |> Segment.orientationToString |> Html.text |> List.singleton |> Ui.overlay Ui.TopLeft |> Ui.debugOnly
            , model.position.path |> List.map (Fold.directionToString >> Html.text) |> Ui.overlay Ui.TopRight |> Ui.debugOnly
            ]
            ++ overlays
            ++ [ Html.map never viewFab ]
            |> List.indexedMap (String.fromInt >> Tuple.pair)
            |> Keyed.node "li"
                (id model.segment.id
                    :: toClass model
                    :: class (Segment.orientationToString (Segment.orientation model.segment))
                    :: class (bodyTypeToString model.segment.body)
                    :: Segment.structureClass config model.segment
                    :: toCssVariables model
                    :: css ownWidthAsVars
                    :: additionalAttributes
                )
            |> Tuple.pair model.segment.id
            |> (\scene -> Ui.fromEmpty (\e -> { e | scene = scene }))
            |> Ui.with ui
