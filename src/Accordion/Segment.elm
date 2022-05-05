module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , view, structureClass, orientationToString, hasBody
    , Action(..), BodyChoice(..), BodyTemplate(..), InfoTemplate(..), Shape(..), Templates, addClass, apply, defaultIllustration, edit, hint, infoLineCount, initialTemplates, isBackground, isIllustration, orientation, removeClass, width
    )

{-| contain the immutable site content

_To render Segments differently based on their position in the tree, use
[`Segment.Viewmode`](Accordion.Segment.ViewMode)_

  - `ViewMode` adds classes based on the position in the tree AND on the screen
  - `Segment` adds classes based on the intended config, independent from the position

@docs Segment
@docs empty, singleton
@docs Orientation


# Map

@docs withIllustration, withContent, withOrientation, withoutCaption, withAdditionalCaption, withInfo, withAdditionalAttributes
@docs decreaseColumnCount, increaseColumnCount


# View

@docs view, structureClass, orientationToString, hasBody

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
import Html.Styled.Keyed as Keyed
import Layout exposing (..)
import Time
import Ui
import Zipper exposing (Zipper)
import Zipper.Branch as Branch
import Zipper.Mixed as MixedZipper exposing (MixedZipper)
import Zipper.Tree as Tree


debugging =
    False


{-| only contains user-editable properties
-}
type alias Segment =
    { id : String
    , caption : { text : String, showsDate : Bool }
    , info : Maybe InfoChoice
    , heading : Maybe String
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
    | WithHeading (Maybe String)
    | WithBody BodyChoice
    | WithShape Shape
    | WithFab (Maybe Fab)
    | WithClasses (List String)


type Shape
    = Oriented Orientation Width
    | Background


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


type InfoTemplate
    = Byline Int (Html Never)
    | Toc


type BodyChoice
    = PeekThrough
    | CustomContent
    | CustomIllustration


type BodyTemplate
    = Content (Html Never)
    | Illustration (Html Never)


{-| This is akin to Msg, with the difference being that Action is serializable
-}
apply : Action -> Segment -> Segment
apply a s =
    case a of
        WithCaption c ->
            { s | caption = c }

        WithHeading h ->
            { s | heading = h }

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


initialTemplates : { body : Dict String ( Bool, BodyTemplate ), info : Dict String ( Bool, InfoTemplate ) }
initialTemplates =
    { body =
        Dict.fromList
            [ ( "intro", Content (Html.text "intro") )
            ]
            |> Dict.map (\_ -> Tuple.pair False)
    , info =
        Dict.empty
            |> Dict.map (\_ -> Tuple.pair False)
    }


{-| -}
isIllustration : { templates : Templates } -> Segment -> Bool
isIllustration { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just (Illustration _) ) ->
            True

        ( CustomIllustration, Nothing ) ->
            True

        _ ->
            False


orientation : Segment -> Orientation
orientation s =
    case s.shape of
        Oriented o _ ->
            o

        _ ->
            Vertical


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
        , id = String.replace " " "-" caption
    }


{-| -}
empty : Segment
empty =
    { id = "_"
    , caption = { text = "", showsDate = False }
    , info = Nothing
    , heading = Nothing
    , body = PeekThrough
    , shape = Oriented Vertical (Columns 1)
    , fab = Nothing
    , additionalClasses = []
    }


{-| -}
defaultIllustration : Segment
defaultIllustration =
    { empty | id = "defaultIllustration", body = CustomIllustration }


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
                    (\pos { heading, id } ->
                        Maybe.map
                            (\entry ->
                                Html.li
                                    [ classList [ ( "focused", pos.isFocus ) ] ]
                                    [ Html.a [ Attributes.target "_self", href ("#" ++ id) ] [ Html.text entry ] ]
                            )
                            heading
                            |> Maybe.withDefault (Html.text "")
                    )
                >> Zipper.flat
                >> Html.ul [ class "toc" ]
            )
        >> Maybe.withDefault (Html.text "Table of Contents -- No Entries")



---- View and helpers ----


{-| In contrast to `view`, we can persist Segment Actions as well as insertions into the Accordion when editing
-}
edit :
    { do : Action -> msg
    , insert : Direction -> msg
    , templates : Templates
    , updateTemplates : (Templates -> Templates) -> msg
    , context : Tree.Split Segment
    }
    -> ViewMode
    -> Segment
    -> ( String, Html msg )
edit { do, insert, templates, updateTemplates, context } ({ position } as mode) s =
    let
        ( overlay, propertySheet ) =
            let
                overlaidButton dir hint_ symbol =
                    Html.button [ onClick (insert dir), title hint_ ] [ Html.span [] [ Html.text symbol ] ]
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

                        options =
                            templateOption
                                ++ [ ( "Illustration", do (WithBody CustomIllustration) )
                                   , ( "Content", do (WithBody CustomContent) )
                                   ]
                                |> Zipper.create
                                    ( "Peek Through", do (WithBody PeekThrough) )
                                    []
                    in
                    ( [ Ui.overlay Ui.Top [ overlaidButton Up "insert empty segment to the top" "+" ]
                      , Ui.overlay Ui.Right [ overlaidButton Right "insert empty segment to the right" "+" ]
                      , Ui.overlay Ui.Bottom [ overlaidButton Down "insert empty segment to the bottom" "+" ]
                      , Ui.overlay Ui.Left [ overlaidButton Left "insert empty segment to the left" "+" ]
                      ]
                    , [ Ui.sheet
                            [ Html.div []
                                [ Ui.pick
                                    options
                                ]
                            ]
                      ]
                    )

                Parent ->
                    ( [ Ui.none ], [ Ui.none ] )

                _ ->
                    ( [ Ui.none ], [ Ui.none ] )
    in
    view_ { templates = templates, context = context } [] (overlay ++ propertySheet) mode s


{-| -}
view : { templates : Templates, context : Tree.Split Segment } -> ViewMode -> Segment -> ( String, Html msg )
view { templates, context } =
    view_ { templates = templates, context = context } [] []


view_ : { templates : Templates, context : Tree.Split Segment } -> List (Html.Attribute msg) -> List (Html msg) -> ViewMode -> Segment -> ( String, Html msg )
view_ { templates, context } attr els mode s =
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
                [ case s.heading of
                    Nothing ->
                        Html.text ""

                    Just h ->
                        Html.a [ href ("#" ++ s.id) ] [ Html.h2 [ class "segment-heading" ] [ Html.text h ] ]
                , case ( template.body, body ) of
                    ( Just (Content c), _ ) ->
                        c

                    ( Just (Illustration i), _ ) ->
                        i

                    ( Nothing, PeekThrough ) ->
                        Html.text ""

                    ( Nothing, CustomIllustration ) ->
                        Html.text "need to load custom illustration"

                    ( Nothing, CustomContent ) ->
                        Html.text "need to load custom content"
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
                              , hasBody s |> ifElse 1 0
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
        [ s.caption |> viewCaption |> Ui.notIf (hasBody s && mode.position.isLeaf && not mode.position.isRoot)
        , s.body |> viewBody
        , s.info |> Ui.ifJust viewInfo
        , orientation s |> orientationToString |> Html.text |> List.singleton |> Ui.overlay Ui.TopLeft |> Ui.debugOnly
        , mode.position.path |> List.map (Fold.viewDirection >> Html.text) |> Ui.overlay Ui.TopRight |> Ui.debugOnly
        ]
        ++ els
        |> Html.li
            (id s.id
                :: ViewMode.toClass mode
                :: class (orientationToString (orientation s))
                :: class (bodyTypeToString s.body)
                :: structureClass s
                :: ViewMode.toCssVariables mode
                :: css ownWidthAsVars
                :: additionalAttributes
                ++ attr
            )
        |> Tuple.pair s.id


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
            "noBody"

        CustomIllustration ->
            "illustration"

        CustomContent ->
            "content"


{-| -}
structureClass : Segment -> Html.Attribute msg
structureClass s =
    classList [ ( "noCaption", s.caption.text == "" ), ( "hasBody", hasBody s ) ]


{-| -}
hasBody : Segment -> Bool
hasBody s =
    case s.body of
        PeekThrough ->
            False

        _ ->
            True
