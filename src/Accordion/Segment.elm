module Accordion.Segment exposing
    ( Segment
    , Region(..)
    , regionToString
    , addWidth, Offset, cumulativeOffset
    , offsetToCssVariables, zeroOffset
    , fab, occ
    , path
    , toCssVariables
    , view, toClass
    , defaultPeekConfig
    )

{-| Display Data as a Article within an `Accordion`.

@docs Segment

---

@docs Region
@docs regionToString


### Width

@docs addWidth, Offset, cumulativeOffset
@docs offsetToCssVariables, zeroOffset


# Deconstruct

@docs fab, occ
@docs path
@docs toCssVariables


# View

@docs view, toClass


# Helpers

@docs defaultPeekConfig

-}

import Article exposing (Action(..), Article, BodyChoice(..), BodyTemplate(..), InfoChoice(..), InfoTemplate(..), Templates, Width(..), bodyTemplateToString, bodyTypeToString, getTemplate, infoTypeToString)
import Article.Fab as Fab exposing (Fab)
import Bool.Extra exposing (ifElse)
import Css exposing (..)
import Directory exposing (Directory)
import Fold exposing (Direction(..), Position, Role(..))
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed as Keyed
import Layout
import List.Extra as List
import Maybe.Extra as Maybe
import Occurrence exposing (Occurrence)
import Snippet
import Time
import Ui exposing (Ui)
import Ui.Layout.Aspect exposing (Aspect(..))
import Zipper
import Zipper.Branch as Branch exposing (Branch)


{-|

    The ViewModel encodes a Article's momentary state for viewing:
    - logical position (in relation to the enclosing Structure)
    - visual position (in relation to the screen)
    - persistent data
    - neighbors (lazy)

-}
type alias Segment =
    { ---- Position
      position : Position
    , region : Region
    , offset : Offset

    ---- Data
    , article : Article

    ---- Lazy Neighbors
    , breadcrumbs : () -> List Article
    , branch : () -> Branch Article
    }


{-| -}
type Region
    = North
    | South
    | West
    | East
    | NearWest
    | NearEast
    | NearNorth
    | NearSouth
    | Center
    | Peek { targetId : String, hint : String }
    | Cache


{-| -}
defaultPeekConfig : { targetId : String, hint : String }
defaultPeekConfig =
    { targetId = "", hint = "" }


{-| -}
path : Segment -> List Direction
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

        NearSouth ->
            "nearSouth"

        NearNorth ->
            "nearNorth"

        Center ->
            "center"

        Peek _ ->
            "peek"

        Cache ->
            "cache"


{-| encode the cumulative shift from the origin in a series of Articles
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


isPeek : Segment -> Bool
isPeek { region } =
    case region of
        Peek _ ->
            True

        _ ->
            False


isParent : Segment -> Bool
isParent =
    path >> (==) [ Up ]


{-| -}
addWidth : { c | templates : Article.Templates } -> Segment -> Bool -> Width -> Offset -> Offset
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


toString : Segment -> String
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
toClass : Segment -> Html.Attribute msg
toClass =
    toString >> class


{-| -}
toCssVariables : Segment -> Html.Attribute msg
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
occ : Segment -> Maybe Occurrence
occ { article, branch, breadcrumbs } =
    article.fab
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
fab : { c | now : Time.Posix } -> Segment -> Maybe Fab
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
toc : { c | templates : Article.Templates } -> Segment -> Maybe ( Html Never, Int )
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
heading : { c | templates : Templates } -> Article -> Maybe String
heading { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just (Content (Just str) _) ) ->
            Just str

        ( CustomContent (Just str), Nothing ) ->
            Just str

        _ ->
            Nothing



---- View ----


byline : { a | templates : Templates } -> Segment -> ( Html Never, Int )
byline ({ templates } as config) model =
    case ( getTemplate .info model.article templates, model.article.info, model.position.role ) of
        ( Just Toc, _, Parent ) ->
            toc config model
                |> Maybe.withDefault ( Html.text "", 0 )

        ( Just (Byline l b), _, Parent ) ->
            ( Html.div [ class "byline" ] [ span [] [ b ] ], l )

        ( Nothing, Just CustomToc, Parent ) ->
            toc config model
                |> Maybe.withDefault ( Html.text "", 0 )

        ( Nothing, Just (CustomByline _), Parent ) ->
            ( Html.text "Todo: Load Custom Byline from Database", 1 )

        _ ->
            ( Html.text "", 0 )


{-| Returns a `Ui` which contains:

  - **scene**: the visible article
  - **control**: the contingent controls to edit the article, and a logically nested scene with overlays for adding siblings

If you add a handle, you can toggle the control.
To toggle a set of segments, put them all into a scene. Any control within a scene is only drawn if the sibling control to that scene is also drawn
(principle of hierarchical contingency)

-}
view :
    { zone : ( String, Time.Zone )
    , now : Time.Posix
    , templates : Templates
    , directory : Directory
    , do : String -> Action -> msg
    , delete : msg
    , reset : String -> msg
    , rename : String -> msg
    , insert : Direction -> msg
    }
    -> Segment
    -> Ui msg
view ({ zone, templates, directory, do, delete, reset, rename, insert } as config) ({ position, article } as model) =
    let
        -- SCENE --
        hideBecauseVeryFarAway =
            model.position.role == Aisle && List.length model.position.path > 5

        template =
            { body = getTemplate .body model.article templates
            , info = getTemplate .info model.article templates
            , isResetting = getTemplate .isResetting model.article templates
            }

        viewCaption cc =
            let
                oneEntry =
                    Layout.header "" model.article.id cc.text

                secondEntry o =
                    Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]
                        [ oneEntry
                        , Occurrence.view (Occurrence.Short (Tuple.second zone) Occurrence.Days) o
                            |> Layout.htmlHeader "" model.article.id
                        ]
            in
            if cc.showsDate then
                model.article.fab
                    |> Maybe.andThen Fab.occurrence
                    |> Maybe.map secondEntry
                    |> Maybe.withDefault oneEntry

            else
                oneEntry

        viewPeekLink () =
            case model.region of
                Peek c ->
                    let
                        previewOccurrences =
                            occ model
                                |> Maybe.map (Occurrence.view (Occurrence.AsList (Tuple.second zone) Occurrence.Days))
                                |> Maybe.withDefault Ui.none
                    in
                    Html.a [ class "peekLink", href c.targetId, title c.hint ]
                        [ Html.h2 [] [ Html.text c.hint ]
                        , previewOccurrences
                        , previewOccurrences
                        ]

                _ ->
                    Ui.none

        viewBody () body =
            Keyed.node "div"
                [ class "body", classList [ ( "illustrative", Article.isIllustration config model.article ) ] ]
            <|
                if
                    Article.isIllustration { templates = templates } model.article
                        || model.position.role
                        == Focus
                        || model.position.role
                        == Parent
                        || model.position.role
                        == Breadcrumb
                        || model.position.role
                        == Aisle
                then
                    [ Tuple.pair "heading" <|
                        case heading config model.article of
                            Just h ->
                                Html.a [ href (Layout.sanitise model.article.id) ] [ Html.h2 [ class "segment-heading" ] [ Html.text h ] ]

                            Nothing ->
                                Html.text ""
                    , Tuple.pair "content" <|
                        if template.isResetting == Just () then
                            Html.node "sync-hypertext" [ attribute "preset" "OVERWRITE", attribute "state" "editing", attribute "data-id" ("_" ++ model.article.id) ] []

                        else
                            case ( template.body, body ) of
                                ( Just (Content _ c), _ ) ->
                                    c directory
                                        |> Snippet.view

                                ( Just (Illustration i), _ ) ->
                                    i directory
                                        |> Snippet.view

                                ( Nothing, PeekThrough ) ->
                                    Html.text ""

                                ( Nothing, CustomIllustration ) ->
                                    Html.node "sync-hypertext" [ attribute "state" "editing", attribute "data-id" ("_" ++ model.article.id) ] []

                                ( Nothing, CustomContent _ ) ->
                                    Html.node "sync-hypertext" [ attribute "state" "editing", attribute "data-id" ("_" ++ model.article.id) ] []
                    ]

                else
                    []

        ( viewByline, bylineHeight ) =
            byline config model

        additionalAttributes =
            model.article.additionalClasses
                |> List.map class

        ownWidthAsVars =
            (case Article.width model.article of
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
                              , Article.hasBody config model.article |> ifElse 1 0
                              )
                            , ( "ownInfoLines"
                              , bylineHeight
                              )
                            ]
                   )

        viewBounds =
            div [ class "bounds" ] []

        viewFab () =
            fab config model
                |> Maybe.map (Fab.view config)
                |> Maybe.withDefault Ui.none

        scene =
            (if hideBecauseVeryFarAway then
                [ ( "caption", model.article.caption |> viewCaption )
                , ( "body", Html.div [ class "body waiting" ] [] )
                , ( "bounds", viewBounds )
                ]

             else
                [ ( "caption", model.article.caption |> viewCaption |> Ui.notIf (Article.hasBody config model.article && model.position.isLeaf && not model.position.isRoot) |> Ui.notIf (isPeek model) )
                , ( "body", model.article.body |> viewBody () )
                , ( "bounds", viewBounds )
                , ( "peekLink", viewPeekLink () )
                , ( "byline", viewByline )
                , ( "orientation", Article.orientation model.article |> Article.orientationToString |> Html.text |> List.singleton |> Ui.overlay Ui.TopLeft |> Ui.debugOnly )
                , ( "path", model.position.path |> List.map (Fold.directionToString >> Html.text) |> Ui.overlay Ui.TopRight |> Ui.debugOnly )
                , ( "fab", viewFab () )
                ]
            )
                |> List.map (Tuple.mapSecond (Html.map never))
                |> Ui.fromFoliage
                |> Ui.wrap
                    (Keyed.node "li"
                        (id model.article.id
                            :: toClass model
                            :: class (Article.orientationToString (Article.orientation model.article))
                            :: class (bodyTypeToString model.article.body)
                            :: Article.structureClass config model.article
                            :: toCssVariables model
                            :: css ownWidthAsVars
                            :: additionalAttributes
                        )
                        >> Tuple.pair model.article.id
                        >> List.singleton
                    )

        -- CONTROL (contingent Ui) --
        addControlIfPresent =
            let
                intend =
                    do article.id

                overlaidButton dir hint_ symbol =
                    button [ onClick (insert dir), title hint_ ] [ span [] [ text symbol ] ]

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
                        overlaidDeleteButton =
                            details [ class "deleteArticle fly-orientation" ]
                                [ summary [] [ Html.span [] [ Html.text "" ] ]
                                , div [ class "ui flying right-aligned bottom-aligned" ]
                                    [ Ui.toggleButton { front = [ span [] [ text "Cut" ] ], title = "Cut this segment" } False Nothing
                                    , Ui.toggleButton { front = [ span [] [ text "Copy" ] ], title = "Copy this segment, excluding its id" } False Nothing
                                    , Ui.toggleButton { front = [ span [] [ text "Paste" ] ], title = "Paste this segment, excluding its id" } False Nothing
                                    , Ui.toggleButton { front = [ span [] [ text "Delete" ] ], title = "Delete this segment" } False (Just delete)
                                    ]
                                ]

                        activeOption =
                            case ( template.body, article.body ) of
                                ( Just t, _ ) ->
                                    bodyTemplateToString t

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
                                        ( { front = [ Html.text str ], title = "Select a Body type for this Article" }
                                        , maybeNot template.body msg
                                        )
                                    )

                        templateOptions =
                            [ ( bodyTemplateToString (Content (Just "") (always Snippet.none)), Nothing )
                            , ( bodyTemplateToString (Illustration (always Snippet.none)), Nothing )
                            ]
                                |> Zipper.create
                                    ( bodyTemplateToString (Content Nothing (always Snippet.none)), Nothing )
                                    []
                                |> Zipper.findClosest (Tuple.first >> (==) activeOption)
                                |> Zipper.map
                                    (\( str, msg ) ->
                                        ( { front = [ Html.text str ], title = "Preset Content type" }
                                        , maybeNot template.body msg
                                        )
                                    )
                    in
                    Ui.with Control
                        ([]
                            |> Ui.with Control
                                (Html.fieldset [ class "ui" ]
                                    [ Html.legend [ class "fill-h" ]
                                        [ Ui.textInput "The Unique ID of this segment" article.id (Just <| \newName -> rename newName)
                                        , Ui.distanceHolder
                                        , overlaidDeleteButton
                                        ]
                                    , case template.body of
                                        Just _ ->
                                            Ui.row
                                                [ Ui.pick (templateOptions |> Zipper.map (Tuple.mapSecond (always Nothing)))
                                                , Ui.toggleButton { front = [ span [] [ text "↺" ] ], title = "Replace the contents with Flupsi's Preset" } False Nothing
                                                ]

                                        Nothing ->
                                            Ui.row
                                                [ Ui.pick options
                                                , Ui.toggleButton { front = [ span [] [ text "↺" ] ], title = "Replace the contents with Flupsi's Preset" } False (Just (reset article.id))
                                                ]
                                    ]
                                    |> Ui.fromHtml
                                )
                            |> Ui.with Scene
                                ([ Ui.overlay Ui.Top [ overlaidButton Up "insert empty segment to the top" "+" ]
                                 , Ui.overlay Ui.Right [ overlaidButton Right "insert empty segment to the right" "+" ]
                                 , Ui.overlay Ui.Bottom [ overlaidButton Down "insert empty segment to the bottom" "+" ]
                                 , Ui.overlay Ui.Left [ overlaidButton Left "insert empty segment to the left" "+" ]
                                 ]
                                    |> Html.li
                                        (toClass model
                                            :: class (Article.orientationToString (Article.orientation model.article))
                                            :: class (bodyTypeToString model.article.body)
                                            :: Article.structureClass config model.article
                                            :: toCssVariables model
                                            :: css ownWidthAsVars
                                            :: additionalAttributes
                                        )
                                    |> Ui.fromHtml
                                )
                        )

                Parent ->
                    let
                        activeOption =
                            case ( template.info, article.info ) of
                                ( Just _, custom ) ->
                                    --"Preset"
                                    infoTypeToString custom

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
                            model.article.caption
                    in
                    Ui.with Control
                        (Html.fieldset [ class "ui" ]
                            [ Html.legend [ class "editCaption fill-h" ]
                                [ Ui.textInput "Caption" article.caption.text (Just <| \txt -> intend (WithCaption { originalCaption | text = txt }))
                                , Ui.distanceHolder
                                , Ui.singlePickOrNot article.caption.showsDate
                                    ( { front = [ Html.text "📅" ], title = "Should the Caption include a date (range)?" }
                                    , Just (intend (WithCaption { originalCaption | showsDate = not model.article.caption.showsDate }))
                                    )
                                ]
                            , Ui.pick options
                            , Fab.edit { zone = zone, save = WithFab >> intend } model.article.fab
                            ]
                            |> Ui.fromHtml
                        )

                _ ->
                    identity
    in
    scene
        |> addControlIfPresent
