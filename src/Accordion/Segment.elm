module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withoutCaption, withInfo
    , view, structureClass, orientationToString, hasBody
    , Action(..), BodyChoice(..), Info(..), Shape(..), Template(..), addClass, apply, decrementColumnCount, decrementInfoLines, defaultIllustration, edit, incrementColumnCount, incrementInfoLines, isIllustration, withBackground, withBody, withShape
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
import Fold exposing (Direction(..), Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Layout exposing (..)
import Time
import Ui
import Zipper


debugging =
    False


{-| -}
type alias Segment =
    { caption : String
    , heading : Maybe String
    , showsDate : Bool
    , id : String
    , isBackground : Bool
    , body : Body
    , info : Maybe ( Int, Html Never )
    , orientation : Orientation
    , width : Width
    , fab : Maybe Fab
    , additionalClasses : List String
    }


type Action
    = WithBodyChoice BodyChoice
    | WithTemplate Template
    | WithHeading (Maybe String)
    | WithInfo Info
    | WithFab Fab
    | WithCaption String
    | IncrementInfoLines
    | DecrementInfoLines
    | IncrementColumnCount
    | DecrementColumnCount
    | AddClass String
    | RemoveClass String
    | ShowsDate Bool
    | WithShape Shape


type Shape
    = Oriented Orientation
    | Background



-- TOC needs to be generated on view so that we only need to store the unit token TOC on the database


type Info
    = Byline String
    | Toc (Html Never)


{-| This is akin to Msg, with the difference being that Action is serializable
-}
apply : Action -> Segment -> Segment
apply a =
    case a of
        WithCaption c ->
            withCaption c

        WithHeading h ->
            withHeading h

        -- Content
        WithTemplate t ->
            withTemplate t

        WithBodyChoice b ->
            withBodyChoice b

        WithInfo (Byline byline) ->
            withInfo (Html.text byline)

        WithInfo (Toc toc) ->
            withInfo toc

        WithFab fab ->
            \segment -> { segment | fab = Just fab }

        -- Layout
        IncrementInfoLines ->
            incrementInfoLines

        DecrementInfoLines ->
            decrementInfoLines

        IncrementColumnCount ->
            incrementColumnCount

        DecrementColumnCount ->
            decrementColumnCount

        AddClass className ->
            addClass className

        RemoveClass className ->
            removeClass className

        ShowsDate bool ->
            showsDate bool

        WithShape s ->
            withShape s


withCaption : String -> Segment -> Segment
withCaption c s =
    { s | caption = c }


withHeading : Maybe String -> Segment -> Segment
withHeading h s =
    { s | heading = h }


{-| -}
isIllustration : Segment -> Bool
isIllustration { body } =
    case body of
        ( CustomIllustration, _ ) ->
            True

        ( Preset, Illustration _ ) ->
            True

        _ ->
            False


{-| -}
defaultIllustration : Segment
defaultIllustration =
    { empty
        | id = "Moving-Across-Thresholds(default)"
        , body =
            Html.div [] [ Html.text "Default Illustration" ]
                |> Illustration
                |> Tuple.pair Preset
    }


{-| Choosing 'Preset' or 'None' will ignore any existing remote content
-}
type alias Body =
    ( BodyChoice, Template )


type BodyChoice
    = Preset
    | NoBody
    | CustomContent
    | CustomIllustration


type Template
    = None -- None means there is a Peek, so here we can add some config for the peek
    | Content (Html Never)
    | Illustration (Html Never)


{-| -}
type Orientation
    = Vertical
    | Horizontal


{-| -}
withShape : Shape -> Segment -> Segment
withShape shape segment =
    case shape of
        Oriented o ->
            { segment | orientation = o, isBackground = False }

        Background ->
            { segment | isBackground = True }


{-| -}
withBackground : Bool -> Segment -> Segment
withBackground isBackground segment =
    { segment | isBackground = isBackground }


{-| -}
withBody : Body -> Segment -> Segment
withBody body segment =
    { segment | body = body }


{-| -}
withTemplate : Template -> Segment -> Segment
withTemplate t s =
    { s | body = Tuple.mapSecond (always t) s.body }


{-| -}
withBodyChoice : BodyChoice -> Segment -> Segment
withBodyChoice b segment =
    { segment | body = Tuple.mapFirst (always b) segment.body }


{-| -}
withInfo : Html Never -> Segment -> Segment
withInfo info segment =
    case segment.info of
        Nothing ->
            { segment | info = Just ( 1, info ) }

        Just ( infoLines, _ ) ->
            { segment | info = Just ( infoLines, info ) }


{-| -}
withoutCaption : Segment -> Segment
withoutCaption segment =
    { segment | caption = "" }


{-| -}
showsDate : Bool -> Segment -> Segment
showsDate bool segment =
    { segment | showsDate = bool }


{-| -}
incrementColumnCount : Segment -> Segment
incrementColumnCount segment =
    { segment
        | width =
            case segment.width of
                Columns n ->
                    Columns (n + 1)

                Screen ->
                    Columns 1
    }


{-| -}
decrementColumnCount : Segment -> Segment
decrementColumnCount segment =
    { segment
        | width =
            case segment.width of
                Columns n ->
                    Columns (n - 1)

                Screen ->
                    Columns 1
    }


{-| -}
incrementInfoLines : Segment -> Segment
incrementInfoLines segment =
    { segment
        | info =
            segment.info
                |> Maybe.map (Tuple.mapFirst ((+) 1))
    }


{-| -}
decrementInfoLines : Segment -> Segment
decrementInfoLines segment =
    { segment
        | info =
            segment.info
                |> Maybe.map (Tuple.mapFirst ((+) -1))
    }


{-| -}
addClass : String -> Segment -> Segment
addClass str segment =
    { segment | additionalClasses = str :: segment.additionalClasses }


{-| -}
removeClass : String -> Segment -> Segment
removeClass str segment =
    { segment | additionalClasses = List.filter ((/=) str) segment.additionalClasses }


{-| -}
singleton : String -> Segment
singleton caption =
    { empty
        | caption = caption
        , id = String.replace " " "-" caption
    }


{-| -}
empty : Segment
empty =
    { caption = ""
    , heading = Nothing
    , showsDate = False
    , id = "_"
    , isBackground = False
    , body = ( Preset, None )
    , orientation = Vertical
    , width = Columns 1
    , additionalClasses = []
    , fab = Nothing
    , info = Nothing
    }



---- View and helpers ----


{-| In contrast to `view`, we can persist Segment Actions as well as insertions into the Accordion when editing
-}
edit : { do : Action -> msg, insert : Direction -> msg } -> ViewMode -> Segment -> ( String, Html msg )
edit { do, insert } ({ position } as mode) s =
    let
        ( overlay, propertySheet ) =
            let
                overlaidButton dir hint symbol =
                    Html.button [ onClick (insert dir), title hint ] [ Html.span [] [ Html.text symbol ] ]
            in
            case position.role of
                Focus ->
                    ( [ Ui.overlay Ui.Top [ overlaidButton Up "insert empty segment to the top" "+" ]
                      , Ui.overlay Ui.Right [ overlaidButton Right "insert empty segment to the right" "+" ]
                      , Ui.overlay Ui.Bottom [ overlaidButton Down "insert empty segment to the bottom" "+" ]
                      , Ui.overlay Ui.Left [ overlaidButton Left "insert empty segment to the left" "+" ]
                      ]
                    , [ Ui.sheet
                            [ Html.div []
                                [ Zipper.create
                                    ( "Preset", do (WithBodyChoice Preset) )
                                    [ ( "None", do (WithBodyChoice NoBody) )
                                    , ( "Illustration", do (WithBodyChoice CustomIllustration) )
                                    , ( "Content", do (WithBodyChoice CustomContent) )
                                    ]
                                    []
                                    |> Ui.pick
                                ]
                            ]
                      ]
                    )

                Parent ->
                    ( [ Ui.none ], [ Ui.none ] )

                _ ->
                    ( [ Ui.none ], [ Ui.none ] )
    in
    view_ [] (overlay ++ propertySheet) mode s


{-| -}
view : ViewMode -> Segment -> ( String, Html msg )
view =
    view_ [] []


view_ : List (Html.Attribute msg) -> List (Html msg) -> ViewMode -> Segment -> ( String, Html msg )
view_ attr els mode s =
    let
        viewCaption cc =
            header "" s.id cc

        --List.map (header "" s.id) cc |> Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]
        viewBody body =
            let
                bodyIsVisible =
                    (||) (isIllustration s) <|
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
                , case body of
                    ( Preset, Illustration illu ) ->
                        case mode.region of
                            ViewMode.Peek config ->
                                Html.a [ href ("#" ++ config.targetId), title config.hint ] [ Html.map never illu ]

                            _ ->
                                Html.map never illu

                    ( Preset, Content content ) ->
                        content

                    ( CustomIllustration, _ ) ->
                        Html.text "need to load custom illustration"

                    ( CustomContent, _ ) ->
                        Html.text "need to load custom content"

                    _ ->
                        Html.div [ css [ maxHeight (px 0), maxWidth (px 0) ] ] []
                ]

             else
                []
            )
                |> Html.div [ class "body" ]

        additionalAttributes =
            s.additionalClasses
                |> List.map Attributes.class

        ownWidthAsVars =
            (\( col, scr ) ->
                List.map Layout.toProperty
                    [ ( "ownColumns", col )
                    , ( "ownScreens", scr )
                    , ( "ownHeaders", hasBody s |> ifElse 1 0 )
                    , ( "ownInfoLines", Maybe.map Tuple.first s.info |> Maybe.withDefault 0 )
                    ]
            )
            <|
                case s.width of
                    Columns c ->
                        ( c, 0 )

                    Screen ->
                        ( 0, 1 )
    in
    Tuple.pair s.id <|
        Html.li
            (id s.id
                :: ViewMode.toClass mode
                :: class (orientationToString s.orientation)
                :: class (bodyTypeToString s.body)
                :: structureClass s
                :: ViewMode.toCssVariables mode
                :: css ownWidthAsVars
                :: additionalAttributes
                ++ attr
            )
        <|
            List.map (Html.map never)
                [ s.caption |> viewCaption |> Ui.notIf (hasBody s && mode.position.isLeaf && not mode.position.isRoot)
                , s.body |> viewBody
                , s.info |> Ui.ifJust (Tuple.second >> List.singleton >> Html.div [ class "info" ])
                , s.orientation |> orientationToString |> Html.text |> List.singleton |> Ui.overlay Ui.TopLeft |> Ui.debugOnly
                , mode.position.path |> List.map (Fold.viewDirection >> Html.text) |> Ui.overlay Ui.TopRight |> Ui.debugOnly
                ]
                ++ els


{-| -}
orientationToString : Orientation -> String
orientationToString orientation =
    case orientation of
        Horizontal ->
            "🀱"

        Vertical ->
            "🁣"


{-| -}
bodyTypeToString : Body -> String
bodyTypeToString body =
    case body of
        ( Preset, Content _ ) ->
            "content"

        ( Preset, Illustration _ ) ->
            "illustration"

        ( CustomContent, _ ) ->
            "content"

        ( CustomIllustration, _ ) ->
            "illustration"

        _ ->
            "noBody"


{-| -}
structureClass : Segment -> Html.Attribute msg
structureClass s =
    classList [ ( "noCaption", s.caption == "" ), ( "hasBody", hasBody s ) ]


{-| -}
hasBody : Segment -> Bool
hasBody s =
    case s.body of
        ( NoBody, _ ) ->
            False

        ( Preset, None ) ->
            False

        _ ->
            True
