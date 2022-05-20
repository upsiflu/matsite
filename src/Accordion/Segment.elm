module Accordion.Segment exposing
    ( Segment
    , defaultIllustration
    , empty, singleton
    , Action(..)
    , actionCodec
    , apply
    , Orientation(..), Shape(..)
    , initialTemplates
    , Templates, BodyTemplate(..), InfoTemplate(..)
    , hint, orientationToString, hasBody, isBackground, isIllustration, width, orientation
    , infoLineCount
    , view, edit
    , fab, templatesAreOn, toc, toggleTemplates
    )

{-| contain the immutable site content

_To render Segments differently based on their position in the tree, use
[`Segment.Viewmode`](Accordion.Segment.ViewMode)_

  - `ViewMode` adds classes based on the position in the tree AND on the screen
  - `Segment` adds classes based on the intended config, independent from the position

@docs Segment


# Create

@docs defaultIllustration
@docs empty, singleton


## Actions

@docs Action
@docs actionCodec
@docs apply


## Field types

@docs Orientation, Shape


## Template types

@docs initialTemplates
@docs Templates, BodyTemplate, InfoTemplate


# Deconstruct

@docs hint, orientationToString, hasBody, isBackground, isIllustration, width, orientation
@docs infoLineCount


# View

@docs view, edit

-}

import Accordion.Segment.Fab as Fab exposing (Fab(..))
import Accordion.Segment.ViewMode as ViewMode exposing (ViewMode, Width(..))
import Bool.Extra as Bool exposing (ifElse)
import Codec exposing (Codec, bool, decoder, field, float, int, maybeField, string, variant0, variant1, variant2)
import Css exposing (..)
import Dict exposing (Dict)
import Fold exposing (Direction(..), Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Time
import Ui exposing (Ui)
import Zipper
import Zipper.Branch as Branch
import Zipper.Mixed as MixedZipper
import Zipper.Tree as Tree


debugging =
    False


{-| only contains user-editable properties
-}
type alias Segment =
    { id : String
    , caption : { text : String, showsDate : Bool }
    , info : Maybe InfoChoice
    , body : BodyChoice
    , shape : Shape
    , fab : Maybe Fab
    , additionalClasses : List String
    }


{-| Actions are encoded and decoded
-}
type Action
    = WithCaption { text : String, showsDate : Bool }
    | WithInfo (Maybe InfoChoice)
    | WithBody BodyChoice
    | WithShape Shape
    | WithFab (Maybe Fab)
    | WithClasses (List String)


{-| -}
actionCodec : Codec Action
actionCodec =
    Codec.custom
        (\cap inf bod sha fab_ cla value ->
            case value of
                WithCaption c ->
                    cap c

                WithInfo i ->
                    inf i

                WithBody b ->
                    bod b

                WithShape s ->
                    sha s

                WithFab f ->
                    fab_ f

                WithClasses c ->
                    cla c
        )
        |> Codec.variant1 "WithCaption"
            WithCaption
            (Codec.object Caption
                |> field "text" .text string
                |> field "showsDate" .showsDate bool
                |> Codec.buildObject
            )
        |> Codec.variant1 "WithInfo" WithInfo (infoCodec |> Codec.maybe)
        |> Codec.variant1 "WithBody" WithBody bodyCodec
        |> Codec.variant1 "WithShape" WithShape shapeCodec
        |> Codec.variant1 "WithFab" WithFab (Fab.codec |> Codec.maybe)
        |> Codec.variant1 "WithClasses" WithClasses (Codec.map (String.split " ") (String.join " ") Codec.string)
        |> Codec.buildCustom


type alias Caption =
    { text : String, showsDate : Bool }


type alias Heading =
    Maybe String


{-| -}
type Shape
    = Oriented Orientation Width
    | Background


{-| -}
shapeCodec : Codec Shape
shapeCodec =
    Codec.custom
        (\oriented background value ->
            case value of
                Oriented o w ->
                    oriented o w

                Background ->
                    background
        )
        |> Codec.variant2 "Oriented" Oriented orientationCodec ViewMode.widthCodec
        |> Codec.variant0 "Background" Background
        |> Codec.buildCustom


{-| -}
orientationCodec : Codec Orientation
orientationCodec =
    Codec.map (orientationFromString >> Maybe.withDefault Vertical) orientationToString Codec.string


decodeOrientation : Decoder Orientation
decodeOrientation =
    let
        recover x =
            case x of
                "Vertical" ->
                    Decode.succeed Vertical

                "Horizontal" ->
                    Decode.succeed Horizontal

                other ->
                    Decode.fail <| "Unknown constructor for type Orientation: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeShape : Decoder Shape
decodeShape =
    Decode.field "Constructor" Decode.string
        |> Decode.andThen
            (\constructor ->
                case constructor of
                    "Oriented" ->
                        Decode.map2
                            Oriented
                            (Decode.field "A1" decodeOrientation)
                            (Decode.field "A2" (decoder ViewMode.widthCodec))

                    "Background" ->
                        Decode.succeed Background

                    other ->
                        Decode.fail <| "Unknown constructor for type Shape: " ++ other
            )


{-| -}
width : Segment -> Width
width s =
    case s.shape of
        Background ->
            Screen

        Oriented _ w ->
            w


{-| option "Preset" is grayed out if templates.info==Nothing
-}
type InfoChoice
    = CustomByline Int
    | CustomToc


infoCodec : Codec InfoChoice
infoCodec =
    Codec.custom
        (\cbyl ctoc value ->
            case value of
                CustomByline i ->
                    cbyl i

                CustomToc ->
                    ctoc
        )
        |> Codec.variant1 "CustomByline" CustomByline Codec.int
        |> Codec.variant0 "CustomToc" CustomToc
        |> Codec.buildCustom


{-| -}
type InfoTemplate
    = Byline Int (Html Never)
    | Toc


type BodyChoice
    = PeekThrough
    | CustomContent Heading
    | CustomIllustration


bodyCodec : Codec BodyChoice
bodyCodec =
    Codec.custom
        (\pee con ill value ->
            case value of
                PeekThrough ->
                    pee

                CustomContent h ->
                    con h

                CustomIllustration ->
                    ill
        )
        |> Codec.variant0 "PeekThrough" PeekThrough
        |> Codec.variant1 "CustomContent" CustomContent (Codec.maybe string)
        |> Codec.variant0 "CustomIllustration" CustomIllustration
        |> Codec.buildCustom


{-| -}
type BodyTemplate
    = Content Heading (Html Never)
    | Illustration (Html Never)


{-| akin to update, but with serializable `Action` instead of `Msg`
-}
apply : Action -> Segment -> Segment
apply a s =
    case a of
        WithCaption c ->
            { s | caption = c }

        -- Content
        WithBody b ->
            { s | body = b }

        WithInfo i ->
            { s | info = i }

        WithFab f ->
            { s | fab = f }

        WithClasses c ->
            { s | additionalClasses = c }

        WithShape h ->
            { s | shape = h }


{-| per-session switchable hardcoded presets; off by default
-}
type alias Templates =
    { body : Dict String ( Bool, BodyTemplate ), info : Dict String ( Bool, InfoTemplate ) }


getTemplate : (Templates -> Dict String ( Bool, v )) -> Segment -> Templates -> Maybe v
getTemplate selector s =
    selector
        >> Dict.get s.id
        >> Maybe.andThen
            (\( activated, t ) ->
                if activated then
                    Just t

                else
                    Nothing
            )


toggleBodyTemplate : Segment -> Templates -> Templates
toggleBodyTemplate s t =
    { t | body = Dict.update s.id (Maybe.map (Tuple.mapFirst not)) t.body }


toggleInfoTemplate : Segment -> Templates -> Templates
toggleInfoTemplate s t =
    { t | info = Dict.update s.id (Maybe.map (Tuple.mapFirst not)) t.info }


{-| Volatile per-session preset dicts; individually switchable; all off by default; see `Data` module for presets
-}
initialTemplates : { body : Dict String ( Bool, BodyTemplate ), info : Dict String ( Bool, InfoTemplate ) }
initialTemplates =
    { body =
        Dict.empty
    , info =
        Dict.empty
    }


{-| -}
orientation : Segment -> Orientation
orientation s =
    case s.shape of
        Oriented o _ ->
            o

        _ ->
            Vertical


{-| -}
isBackground : Segment -> Bool
isBackground s =
    case s.shape of
        Background ->
            True

        _ ->
            False


{-| -}
type Orientation
    = Vertical
    | Horizontal


{-| -}
addClass : String -> Segment -> Action
addClass str segment =
    WithClasses (str :: segment.additionalClasses)


{-| -}
removeClass : String -> Segment -> Action
removeClass str segment =
    WithClasses (List.filter ((/=) str) segment.additionalClasses)


{-| -}
singleton : String -> Segment
singleton caption =
    { empty
        | caption = { text = caption, showsDate = False }
        , id = Layout.sanitise caption
    }


{-| -}
empty : Segment
empty =
    { id = ""
    , caption = { text = "", showsDate = False }
    , info = Nothing
    , body = PeekThrough
    , shape = Oriented Vertical (Columns 1)
    , fab = Nothing
    , additionalClasses = []
    }


{-| -}
defaultIllustration : Segment
defaultIllustration =
    { empty | id = "defaultIllustration", body = CustomIllustration }


{-| Title for peeks and images. May be extended later to include A11y captions.
-}
hint : Segment -> String
hint s =
    s.caption.text
        ++ (if s.caption.showsDate then
                " (TODO: Date)"

            else
                ""
           )


{-| the key to neighbors; requested lazily in the `P` segment.

toc : yields the zipper of the headings of the present (a.k.a. the Aisle)

occ : yields the embracing occasion around all occurrences of the `future`

-}
toc : { c | templates : Templates, context : Tree.Split Segment } -> ( Html Never, Int )
toc config =
    config.context.present
        |> MixedZipper.mapPeriphery Branch.node
        |> (\({ focus } as zipper) ->
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
           )
        |> Zipper.flat
        |> List.filterMap identity
        |> (\l -> ( Html.ul [ class "info toc" ] l, List.length l // 6 ))


{-|

    1. The Fab in the focus, if in the future
    2. Otherwise, Out of all Fabs in the Descendants that are in the future, the one with the earliest starting date
    3. Otherwise, Going up the breadcrumbs, take the first Fab, if any

-}
fab : { c | templates : Templates, context : Tree.Split Segment, now : Time.Posix } -> Maybe Fab
fab config =
    --1. The Fab in the focus, and in the future
    config.context.present.focus.fab
        |> Maybe.filter (Fab.isActive config)
        --2. Otherwise, Out of all Fabs in the Descendants that are in the future, the one with the earliest starting date
        |> Maybe.or
            (config.context.future
                |> List.concatMap
                    (MixedZipper.concatMap
                        List.singleton
                        Branch.flat
                    )
                -- List Segment
                |> List.filter
                    (.fab >> Maybe.map (Fab.isActive config) >> Maybe.withDefault False)
                |> List.minimumBy
                    (.fab >> Maybe.andThen Fab.beginning >> Maybe.map Time.posixToMillis >> Maybe.withDefault 2147483646)
                |> Maybe.andThen .fab
            )



{-
   |> MixedZipper.mapPeriphery Branch.node
   |> (\({ focus } as zipper) ->
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
      )
   |> Zipper.flat
   |> List.filterMap identity
   |> (\l -> ( Html.ul [ class "info toc" ] l, List.length l // 6 ))
-}


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



---- View and helpers ----


{-| In contrast to `view`, we can persist Segment Actions as well as insertions into the Accordion when editing
-}
edit :
    { zone : Maybe ( String, Time.Zone )
    , do : String -> Action -> msg
    , delete : msg
    , rename : String -> msg
    , insert : Direction -> msg
    , templates : Templates
    , context : Tree.Split Segment
    , now : Time.Posix
    }
    -> ViewMode
    -> Segment
    -> Ui msg
edit { zone, do, insert, delete, rename, templates, context, now } ({ position } as mode) s =
    let
        intend =
            do s.id

        ( overlay, propertySheet ) =
            let
                overlaidButton dir hint_ symbol =
                    Html.button [ onClick (insert dir), title hint_ ] [ Html.span [] [ Html.text symbol ] ]

                overlaidDeleteButton =
                    Html.details [ class "deleteSegment fly-orientation" ]
                        [ Html.summary [] [ Html.span [] [ Html.text "" ] ]
                        , Html.div [ class "ui flying right-aligned bottom-aligned" ]
                            [ Ui.toggleButton { front = [ Html.span [] [ Html.text "Cut" ] ], title = "Cut this segment" } False Nothing
                            , Ui.toggleButton { front = [ Html.span [] [ Html.text "Copy" ] ], title = "Copy this segment, excluding its id" } False Nothing
                            , Ui.toggleButton { front = [ Html.span [] [ Html.text "Paste" ] ], title = "Paste this segment, excluding its id" } False Nothing
                            , Ui.toggleButton { front = [ Html.span [] [ Html.text "Delete" ] ], title = "Delete this segment" } False (Just delete)
                            ]
                        ]

                template =
                    { body = getTemplate .body s templates
                    , info = getTemplate .info s templates
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
                            case ( template.body, s.body ) of
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
                            [ Ui.textInput "The Unique ID of this segment" s.id (Just <| \newName -> rename newName), Ui.distanceHolder, overlaidDeleteButton ]
                        , Ui.pick options
                        ]
                    )

                Parent ->
                    let
                        activeOption =
                            case ( template.info, s.info ) of
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
                            s.caption
                    in
                    ( []
                    , Html.fieldset [ class "ui" ]
                        [ Html.legend [ class "editCaption" ]
                            [ Ui.textInput "Caption" s.caption.text (Just <| \txt -> intend (WithCaption { originalCaption | text = txt }))
                            , Ui.singlePickOrNot s.caption.showsDate
                                ( { front = [ Html.text "📅" ], title = "Should the Caption include a date (range)?" }
                                , Just (intend (WithCaption { originalCaption | showsDate = not s.caption.showsDate }))
                                )
                            ]
                        , Ui.pick options
                        , Fab.edit { zone = zone, save = WithFab >> intend } s.fab
                        ]
                    )

                _ ->
                    ( [], Ui.none )

        ui =
            Ui.fromEmpty <| \e -> { e | control = propertySheet }
    in
    view_ { zone = zone, templates = templates, context = context, now = now } ui overlay mode s


{-| -}
view :
    { c | zone : Maybe ( String, Time.Zone ), templates : Templates, context : Tree.Split Segment, now : Time.Posix }
    -> ViewMode
    -> Segment
    -> Ui msg
view config =
    view_ config (Ui.fromEmpty identity) []


view_ :
    { c | templates : Templates, context : Tree.Split Segment, zone : Maybe ( String, Time.Zone ), now : Time.Posix }
    -> Ui msg
    -> List (Html msg)
    -> ViewMode
    -> Segment
    -> Ui msg
view_ ({ templates } as config) ui overlays mode s =
    let
        hideBecauseVeryFarAway =
            case mode.position.role of
                Aisle ->
                    if List.length mode.position.path > 5 then
                        True

                    else
                        False

                _ ->
                    False

        viewFab =
            fab config
                |> Maybe.map2 Tuple.pair config.zone
                |> Maybe.map
                    (\( zone, f ) ->
                        case mode.position.role of
                            Parent ->
                                Fab.view { zone = zone } f
                                    |> Html.map never

                            _ ->
                                Ui.none
                    )
                |> Maybe.withDefault Ui.none

        template =
            { body = getTemplate .body s templates
            , info = getTemplate .info s templates
            }

        viewCaption cc =
            header "" s.id cc.text

        viewPeekLink =
            case mode.region of
                ViewMode.Peek c ->
                    Html.a [ class "peekLink", href c.targetId, title c.hint ] [ Html.span [] [ Html.text c.hint ] ]

                _ ->
                    Ui.none

        --List.map (header "" s.id) cc |> Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]
        viewBody body =
            let
                bodyIsVisible =
                    (||) (isIllustration { templates = templates } s) <|
                        case mode.position.role of
                            Focus ->
                                True

                            Parent ->
                                True

                            Breadcrumb ->
                                True

                            Aisle ->
                                True

                            _ ->
                                False
            in
            (if bodyIsVisible then
                [ case heading config s of
                    Nothing ->
                        Html.text ""

                    Just h ->
                        Html.a [ href (Layout.sanitise s.id) ] [ Html.h2 [ class "segment-heading" ] [ Html.text h ] ]
                , case ( template.body, body ) of
                    ( Just (Content _ c), _ ) ->
                        c

                    ( Just (Illustration i), _ ) ->
                        i

                    ( Nothing, PeekThrough ) ->
                        Html.text ""

                    ( Nothing, CustomIllustration ) ->
                        Html.node "sync-hypertext" [ attribute "state" "editing", attribute "data-id" ("_" ++ s.id) ] []

                    ( Nothing, CustomContent _ ) ->
                        Html.node "sync-hypertext" [ attribute "state" "editing", attribute "data-id" ("_" ++ s.id) ] []
                ]

             else
                []
            )
                |> Html.div [ class "body", classList [ ( "illustrative", isIllustration config s ) ] ]

        ( viewByline, bylineHeight ) =
            case ( template.info, s.info, mode.position.role ) of
                ( Just Toc, _, Parent ) ->
                    toc config

                ( Just (Byline l b), _, Parent ) ->
                    ( b, l )

                ( Nothing, Just CustomToc, Parent ) ->
                    toc config

                ( Nothing, Just (CustomByline _), Parent ) ->
                    ( Html.text "Todo: Load Custom Byline from Database", 1 )

                _ ->
                    ( Html.text "", 0 )

        additionalAttributes =
            s.additionalClasses
                |> List.map Attributes.class

        ownWidthAsVars =
            (case width s of
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
                              , hasBody config s |> ifElse 1 0
                              )
                            , ( "ownInfoLines"
                              , bylineHeight
                              )
                            ]
                   )
    in
    if hideBecauseVeryFarAway then
        List.map (Html.map never)
            [ s.caption |> viewCaption
            , Html.div [ class "body waiting" ] []
            ]
            |> Html.li
                (id s.id
                    :: ViewMode.toClass mode
                    :: class (orientationToString (orientation s))
                    :: class (bodyTypeToString s.body)
                    :: structureClass config s
                    :: ViewMode.toCssVariables mode
                    :: css ownWidthAsVars
                    :: additionalAttributes
                )
            |> Tuple.pair s.id
            |> (\scene -> Ui.fromEmpty (\e -> { e | scene = scene }))

    else
        List.map (Html.map never)
            [ s.caption |> viewCaption |> Ui.notIf (hasBody config s && mode.position.isLeaf && not mode.position.isRoot)
            , s.body |> viewBody
            , viewPeekLink
            , viewByline
            , orientation s |> orientationToString |> Html.text |> List.singleton |> Ui.overlay Ui.TopLeft |> Ui.debugOnly
            , mode.position.path |> List.map (Fold.directionToString >> Html.text) |> Ui.overlay Ui.TopRight |> Ui.debugOnly
            ]
            ++ overlays
            ++ [ viewFab ]
            |> Html.li
                (id s.id
                    :: ViewMode.toClass mode
                    :: class (orientationToString (orientation s))
                    :: class (bodyTypeToString s.body)
                    :: structureClass config s
                    :: ViewMode.toCssVariables mode
                    :: css ownWidthAsVars
                    :: additionalAttributes
                )
            |> Tuple.pair s.id
            |> (\scene -> Ui.fromEmpty (\e -> { e | scene = scene }))
            |> Ui.with ui



---- Deconstruct


{-| -}
structureClass : { c | templates : Templates } -> Segment -> Html.Attribute msg
structureClass config s =
    classList [ ( "noCaption", s.caption.text == "" ), ( "hasBody", hasBody config s ) ]


{-| -}
hasBody : { c | templates : Templates } -> Segment -> Bool
hasBody { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just _ ) ->
            True

        ( PeekThrough, Nothing ) ->
            False

        _ ->
            True


{-| -}
isIllustration : { c | templates : Templates } -> Segment -> Bool
isIllustration { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just (Illustration _) ) ->
            True

        ( CustomIllustration, Nothing ) ->
            True

        _ ->
            False


{-| -}
infoLineCount : { c | context : Tree.Split Segment, templates : Templates } -> ViewMode -> Segment -> Int
infoLineCount config mode =
    byline config mode >> Tuple.second


{-| -}
byline : { c | context : Tree.Split Segment, templates : Templates } -> ViewMode -> Segment -> ( Html Never, Int )
byline config mode s =
    case ( getTemplate .info s config.templates, s.info, mode.position.role ) of
        ( Just Toc, _, Parent ) ->
            toc config

        ( Just (Byline l b), _, Parent ) ->
            ( b, l )

        ( Nothing, Just CustomToc, Parent ) ->
            toc config

        ( Nothing, Just (CustomByline _), Parent ) ->
            ( Html.text "Todo: Load Custom Byline from Database", 1 )

        _ ->
            ( Html.text "", 0 )



---- Helpers


{-| -}
orientationFromString : String -> Maybe Orientation
orientationFromString o =
    case o of
        "🀱" ->
            Just Horizontal

        "🁣" ->
            Just Vertical

        _ ->
            Nothing


{-| -}
orientationToString : Orientation -> String
orientationToString o =
    case o of
        Horizontal ->
            "🀱"

        Vertical ->
            "🁣"


{-| -}
bodyTypeToString : BodyChoice -> String
bodyTypeToString body =
    case body of
        PeekThrough ->
            "Peek-Through"

        CustomIllustration ->
            "Illustration"

        CustomContent Nothing ->
            "Html"

        CustomContent (Just n) ->
            "+" ++ n


{-| -}
infoTypeToString : Maybe InfoChoice -> String
infoTypeToString info =
    case info of
        Nothing ->
            "No byline"

        Just CustomToc ->
            "T.o.C."

        Just (CustomByline 1) ->
            "Custom line"

        Just (CustomByline n) ->
            String.fromInt n


toggleTemplates : Templates -> Templates
toggleTemplates t =
    { body = Dict.map (\_ ( b, x ) -> ( not b, x )) t.body
    , info = Dict.map (\_ ( b, x ) -> ( not b, x )) t.info
    }


templatesAreOn : Templates -> Maybe Bool
templatesAreOn =
    .body
        >> Dict.values
        >> List.head
        >> Maybe.map Tuple.first
