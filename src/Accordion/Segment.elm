module Accordion.Segment exposing
    ( Segment
    , defaultIllustration
    , empty, singleton
    , Action(..)
    , encodeAction, decodeAction
    , apply
    , Orientation(..), Shape(..)
    , initialTemplates
    , Templates, BodyTemplate(..), InfoTemplate(..)
    , hint, orientationToString, hasBody, isBackground, isIllustration, width, orientation
    , infoLineCount
    , view, edit
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
@docs encodeAction, decodeAction
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
import Bool.Extra exposing (ifElse)
import Css exposing (..)
import Dict exposing (Dict)
import Fold exposing (Direction(..), Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Layout exposing (..)
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


type alias Caption =
    { text : String, showsDate : Bool }


{-| -}
decodeAction : Decoder Action
decodeAction =
    Decode.field "Constructor" Decode.string
        |> Decode.andThen
            (\constructor ->
                case constructor of
                    "WithCaption" ->
                        Decode.map
                            WithCaption
                            (Decode.field "A1" decodeCaption)

                    "WithInfo" ->
                        Decode.map
                            WithInfo
                            (Decode.field "A1" (Decode.maybe decodeInfoChoice))

                    "WithBody" ->
                        Decode.map
                            WithBody
                            (Decode.field "A1" decodeBodyChoice)

                    "WithShape" ->
                        Decode.map
                            WithShape
                            (Decode.field "A1" decodeShape)

                    "WithFab" ->
                        Decode.map
                            WithFab
                            (Decode.field "A1" (Decode.maybe Fab.decode))

                    "WithClasses" ->
                        Decode.map
                            WithClasses
                            (Decode.field "A1" (Decode.list Decode.string))

                    other ->
                        Decode.fail <| "Unknown constructor for type Action: " ++ other
            )


type alias Heading =
    Maybe String


decodeHeading : Decoder Heading
decodeHeading =
    Decode.maybe Decode.string


decodeBodyChoice : Decoder BodyChoice
decodeBodyChoice =
    let
        recover x =
            case x of
                "PeekThrough" ->
                    Decode.succeed PeekThrough

                "CustomContent" ->
                    Decode.map
                        CustomContent
                        (Decode.field "A1" decodeHeading)

                "CustomIllustration" ->
                    Decode.succeed CustomIllustration

                other ->
                    Decode.fail <| "Unknown constructor for type BodyChoice: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeInfoChoice : Decoder InfoChoice
decodeInfoChoice =
    Decode.field "Constructor" Decode.string
        |> Decode.andThen
            (\constructor ->
                case constructor of
                    "CustomByline" ->
                        Decode.map
                            CustomByline
                            (Decode.field "A1" Decode.int)

                    "CustomToc" ->
                        Decode.succeed CustomToc

                    other ->
                        Decode.fail <| "Unknown constructor for type InfoChoice: " ++ other
            )


decodeCaption : Decoder Caption
decodeCaption =
    Decode.map2
        Caption
        (Decode.field "text" Decode.string)
        (Decode.field "showsDate" Decode.bool)


{-| -}
encodeAction : Action -> Value
encodeAction a =
    case a of
        WithCaption a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithCaption" )
                , ( "A1", encodeRecord_text_String_showsDate_Bool_ a1 )
                ]

        WithInfo a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithInfo" )
                , ( "A1", encodeMaybe encodeInfoChoice a1 )
                ]

        WithBody a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithBody" )
                , ( "A1", encodeBodyChoice a1 )
                ]

        WithShape a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithShape" )
                , ( "A1", encodeShape a1 )
                ]

        WithFab a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithFab" )
                , ( "A1", encodeMaybe Fab.encode a1 )
                ]

        WithClasses a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "WithClasses" )
                , ( "A1", Encode.list Encode.string a1 )
                ]


encodeBodyChoice : BodyChoice -> Value
encodeBodyChoice a =
    case a of
        PeekThrough ->
            Encode.string "PeekThrough"

        CustomContent a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CustomContent" )
                , ( "A1", encodeHeading a1 )
                ]

        CustomIllustration ->
            Encode.string "CustomIllustration"


encodeHeading : Heading -> Value
encodeHeading a =
    encodeMaybe Encode.string a


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe f a =
    case a of
        Just b ->
            f b

        Nothing ->
            Encode.null


encodeInfoChoice : InfoChoice -> Value
encodeInfoChoice a =
    case a of
        CustomByline lineCount ->
            Encode.object
                [ ( "Constructor", Encode.string "CustomByline" )
                , ( "A1", Encode.int lineCount )
                ]

        CustomToc ->
            Encode.object
                [ ( "Constructor", Encode.string "CustomToc" )
                ]


encodeRecord_text_String_showsDate_Bool_ a =
    Encode.object
        [ ( "text", Encode.string a.text )
        , ( "showsDate", Encode.bool a.showsDate )
        ]


{-| -}
type Shape
    = Oriented Orientation Width
    | Background


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
                            (Decode.field "A2" ViewMode.decodeWidth)

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


{-| -}
infoLineCount : Segment -> Int
infoLineCount s =
    case s.info of
        Nothing ->
            0

        Just (CustomByline c) ->
            c

        Just CustomToc ->
            2


{-| -}
type InfoTemplate
    = Byline Int (Html Never)
    | Toc


type BodyChoice
    = PeekThrough
    | CustomContent Heading
    | CustomIllustration


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


encodeOrientation : Orientation -> Value
encodeOrientation a =
    Encode.string <|
        case a of
            Vertical ->
                "Vertical"

            Horizontal ->
                "Horizontal"


encodeShape : Shape -> Value
encodeShape a =
    case a of
        Oriented a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Oriented" )
                , ( "A1", encodeOrientation a1 )
                , ( "A2", ViewMode.encodeWidth a2 )
                ]

        Background ->
            Encode.object
                [ ( "Constructor", Encode.string "Background" )
                ]


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
        , id = String.replace " " "-" caption
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

TocEntries : yields the zipper of the headings of the `(List.head future)`

Occasion : yields the embracing occasion around all occurrences of the `future`

ClosestFab : yields the Fab that is stored in the `present`ly focused segment or up the focuses of the `past`

-}
toc : Tree.Split Segment -> Html Never
toc =
    .future
        >> List.head
        >> Maybe.map
            (MixedZipper.mapPeriphery Branch.node
                >> Zipper.mapByPosition
                    (\pos segment ->
                        Maybe.map
                            (\entry ->
                                Html.li
                                    [ classList [ ( "focused", pos.isFocus ) ] ]
                                    [ Html.a [ Attributes.target "_self", href (Layout.sanitise segment.id) ] [ Html.text entry ] ]
                            )
                            (heading segment)
                            |> Maybe.withDefault (Html.text "")
                    )
                >> Zipper.flat
                >> Html.ul [ class "toc" ]
            )
        >> Maybe.withDefault (Html.text "Table of Contents -- No Entries")


heading : Segment -> Maybe String
heading s =
    case s.body of
        CustomContent (Just str) ->
            Just str

        _ ->
            Nothing



---- View and helpers ----


{-| In contrast to `view`, we can persist Segment Actions as well as insertions into the Accordion when editing
-}
edit :
    { zone : Maybe ( String, Time.Zone )
    , do : Action -> msg
    , delete : msg
    , insert : Direction -> msg
    , templates : Templates
    , updateTemplates : (Templates -> Templates) -> msg
    , context : Tree.Split Segment
    }
    -> ViewMode
    -> Segment
    -> Ui msg
edit { zone, do, insert, delete, templates, updateTemplates, context } ({ position } as mode) s =
    let
        ( overlay, propertySheet ) =
            let
                overlaidButton dir hint_ symbol =
                    Html.button [ onClick (insert dir), title hint_ ] [ Html.span [] [ Html.text symbol ] ]

                overlaidDeleteButton =
                    Html.details [ class "deleteSegment" ]
                        [ Html.summary [] [ Html.span [] [ Html.text "⌫" ] ]
                        , Html.div []
                            [ Html.label [] [ Html.text <| "Delete segment '" ++ s.id ++ "'?" ]
                            , Html.button [ class "deleteSegment", onClick delete, title "delete this segment" ] [ Html.span [] [ Html.text "Yes" ] ]
                            ]
                        ]
            in
            case position.role of
                Focus ->
                    let
                        template =
                            { body = getTemplate .body s templates
                            , info = getTemplate .info s templates
                            }

                        templateOption =
                            Maybe.map (always [ ( "Preset", toggleBodyTemplate s |> updateTemplates ) ]) template.body
                                |> Maybe.withDefault []

                        activeOption =
                            case ( getTemplate .body s templates, s.body ) of
                                ( Just _, _ ) ->
                                    "Preset"

                                ( _, custom ) ->
                                    bodyTypeToString custom

                        options =
                            [ ( bodyTypeToString CustomIllustration, do (WithBody CustomIllustration) )
                            , ( bodyTypeToString (CustomContent Nothing), do (WithBody (CustomContent Nothing)) )
                            , ( bodyTypeToString (CustomContent (Just "Heading")), do (WithBody (CustomContent (Just "Heading"))) )
                            ]
                                |> Zipper.create
                                    ( bodyTypeToString PeekThrough, do (WithBody PeekThrough) )
                                    templateOption
                                |> Zipper.findClosest (Tuple.first >> (==) activeOption)
                    in
                    ( [ Ui.overlay Ui.Top [ overlaidButton Up "insert empty segment to the top" "+" ]
                      , Ui.overlay Ui.TopRight [ overlaidDeleteButton ]
                      , Ui.overlay Ui.Right [ overlaidButton Right "insert empty segment to the right" "+" ]
                      , Ui.overlay Ui.Bottom [ overlaidButton Down "insert empty segment to the bottom" "+" ]
                      , Ui.overlay Ui.Left [ overlaidButton Left "insert empty segment to the left" "+" ]
                      ]
                    , Html.div []
                        [ Ui.pick
                            options
                        ]
                    )

                Parent ->
                    ( [], Fab.edit { zone = zone, save = WithFab >> do } s.fab )

                _ ->
                    ( [], Ui.none )

        ui =
            Ui.fromEmpty (\e -> { e | control = propertySheet })
    in
    view_ { templates = templates, context = context } ui overlay mode s


{-| -}
view :
    { templates : Templates, context : Tree.Split Segment }
    -> ViewMode
    -> Segment
    -> Ui msg
view { templates, context } =
    view_ { templates = templates, context = context } (Ui.fromEmpty identity) []


view_ :
    { templates : Templates, context : Tree.Split Segment }
    -> Ui msg
    -> List (Html msg)
    -> ViewMode
    -> Segment
    -> Ui msg
view_ ({ templates, context } as config) ui overlays mode s =
    let
        viewCaption cc =
            header "" s.id cc.text

        --List.map (header "" s.id) cc |> Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]
        viewBody body =
            let
                template =
                    { body = getTemplate .body s templates
                    , info = getTemplate .info s templates
                    }

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
                [ case heading s of
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
                |> Html.div [ class "body" ]

        viewByline =
            case ( s.info, mode.position.role ) of
                ( Just (CustomByline _), Parent ) ->
                    Html.text "Todo: Load Custom Byline from Database"

                ( Just CustomToc, Parent ) ->
                    toc context

                _ ->
                    Html.text ""

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
                              , mode.position.role == Parent |> ifElse (infoLineCount s) 0
                              )
                            ]
                   )

        viewInfo : InfoChoice -> Html Never
        viewInfo i =
            Html.div [ class "info" ] <|
                case i of
                    CustomByline _ ->
                        [ Html.text "Custom Byline" ]

                    CustomToc ->
                        [ toc context ]
    in
    List.map (Html.map never)
        [ s.caption |> viewCaption |> Ui.notIf (hasBody config s && mode.position.isLeaf && not mode.position.isRoot)
        , s.body |> viewBody
        , s.info |> Ui.ifJust viewInfo
        , orientation s |> orientationToString |> Html.text |> List.singleton |> Ui.overlay Ui.TopLeft |> Ui.debugOnly
        , mode.position.path |> List.map (Fold.directionToString >> Html.text) |> Ui.overlay Ui.TopRight |> Ui.debugOnly
        ]
        ++ overlays
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



---- Helpers


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
