module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withIllustration, withContent, withOrientation, withoutCaption, withAdditionalCaption, withInfo, withAdditionalAttributes
    , decreaseColumnCount, increaseColumnCount
    , view, structureClass, orientationToString, hasBody
    , Action(..), Body(..), Fab(..), Info(..), Template(..), apply, decreaseInfoLines, defaultIllustration, edit, increaseInfoLines, isIllustration, withBackground, withBody
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

import Accordion.Segment.ViewMode as ViewMode exposing (ViewMode, Width(..))
import Bool.Extra exposing (ifElse)
import Css exposing (..)
import Fold exposing (Direction(..), Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Layout exposing (..)
import Occurrence exposing (Occurrence)
import Time
import View
import Zipper


debugging =
    False


{-| -}
type alias Segment =
    { caption : List String
    , id : String
    , isBackground : Bool
    , body : Body
    , info : Maybe ( Int, Html Never )
    , orientation : Orientation
    , width : Width
    , fab : Maybe Fab
    , additionalAttributes : List (Html.Attribute Never)
    }


type Action
    = WithBody Body
    | WithInfo Info
    | AddInfoLine
    | WithFab Fab
    | AddColumn
    | RemoveColumn
    | AddClass String
    | AsBackground Bool
    | MakeHorizontal Bool


type Info
    = Byline String
    | Toc (Html Never)


type Fab
    = Register { link : String, occurance : Occurrence }
    | Subscribe { link : String }


{-| This is akin to Msg, with the difference being that Action is serializable
-}
apply : Action -> Segment -> Segment
apply a =
    case a of
        WithBody body ->
            withBody body

        WithInfo (Byline byline) ->
            withInfo (Html.text byline)

        WithInfo (Toc toc) ->
            withInfo toc

        AddInfoLine ->
            increaseInfoLines

        WithFab fab ->
            \segment -> { segment | fab = Just fab }

        AddColumn ->
            increaseColumnCount

        RemoveColumn ->
            decreaseColumnCount

        AddClass className ->
            withAdditionalAttributes [ class className ]

        AsBackground bool ->
            withBackground bool

        MakeHorizontal bool ->
            if bool then
                withOrientation Horizontal

            else
                withOrientation Vertical


{-| -}
isIllustration : Segment -> Bool
isIllustration { body } =
    case body of
        CustomIllustration _ ->
            True

        Preset (Illustration _) ->
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
                |> Preset
    }


{-| Choosing 'Preset' or 'None' will ignore any existing remote content
-}
type Body
    = Preset Template
    | None (Maybe Template) -- None means there is a Peek, so here we can add some config for the peek
    | CustomContent (Maybe Template)
    | CustomIllustration (Maybe Template)


type Template
    = Content (Html Never)
    | Illustration (Html Never)


{-| -}
type Orientation
    = Vertical
    | Horizontal


{-| -}
withOrientation : Orientation -> Segment -> Segment
withOrientation orientation segment =
    { segment | orientation = orientation }


{-| -}
withBackground : Bool -> Segment -> Segment
withBackground isBackground segment =
    { segment | isBackground = isBackground }


{-| -}
withBody : Body -> Segment -> Segment
withBody body segment =
    { segment | body = body }


{-| -}
withIllustration : Html Never -> Segment -> Segment
withIllustration body segment =
    { segment | body = Illustration body |> Preset }


{-| -}
withContent : Html Never -> Segment -> Segment
withContent body segment =
    { segment | body = Content body |> Preset }


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
    { segment | caption = [] }


{-| -}
withAdditionalCaption : String -> Segment -> Segment
withAdditionalCaption string segment =
    { segment | caption = segment.caption ++ [ string ] }


{-| -}
increaseColumnCount : Segment -> Segment
increaseColumnCount segment =
    { segment
        | width =
            case segment.width of
                Columns n ->
                    Columns (n + 1)

                Screen ->
                    Columns 1
    }


{-| -}
decreaseColumnCount : Segment -> Segment
decreaseColumnCount segment =
    { segment
        | width =
            case segment.width of
                Columns n ->
                    Columns (n - 1)

                Screen ->
                    Columns 1
    }


{-| -}
increaseInfoLines : Segment -> Segment
increaseInfoLines segment =
    { segment
        | info =
            segment.info
                |> Maybe.map (Tuple.mapFirst ((+) 1))
    }


{-| -}
decreaseInfoLines : Segment -> Segment
decreaseInfoLines segment =
    { segment
        | info =
            segment.info
                |> Maybe.map (Tuple.mapFirst ((+) -1))
    }


{-| -}
withAdditionalAttributes : List (Html.Attribute Never) -> Segment -> Segment
withAdditionalAttributes cc segment =
    { segment | additionalAttributes = cc ++ segment.additionalAttributes }


{-| -}
singleton : String -> Segment
singleton id =
    { empty
        | caption = [ id ]
        , id = String.replace " " "-" id
    }


{-| -}
empty : Segment
empty =
    { caption = []
    , id = "_"
    , isBackground = False
    , body = None Nothing
    , orientation = Vertical
    , width = Columns 1
    , additionalAttributes = []
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
            case position.role of
                Focus ->
                    ( [ View.overlay View.Top [ Html.button [ onClick (insert Up), title "insert empty segment to the top" ] [ Html.text "✦" ] ]
                      , View.overlay View.Right [ Html.button [ onClick (insert Right), title "insert empty segment to the right" ] [ Html.text "✦" ] ]
                      , View.overlay View.Bottom [ Html.button [ onClick (insert Down), title "insert empty segment to the bottom" ] [ Html.text "✦" ] ]
                      , View.overlay View.Left [ Html.button [ onClick (insert Left), title "insert empty segment to the left" ] [ Html.text "✦" ] ]
                      ]
                    , [ View.sheet
                            [ Html.div []
                                [ Zipper.create
                                    ( "Small", do (WithBody (None Nothing)) )
                                    [ ( "Medium", do (WithBody (None Nothing)) )
                                    , ( "Large", do (WithBody (None Nothing)) )
                                    ]
                                    []
                                    |> View.pick
                                ]
                            ]
                      ]
                    )

                Parent ->
                    ( [ View.none ], [ View.none ] )

                _ ->
                    ( [ View.none ], [ View.none ] )
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
            case cc of
                [] ->
                    viewCaption [ "⋮" ]

                [ one ] ->
                    header "" s.id one

                _ ->
                    List.map (header "" s.id) cc
                        |> Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]

        viewBody body =
            [ case body of
                Preset (Illustration illu) ->
                    case mode.region of
                        ViewMode.Peek config ->
                            Html.a [ href ("#" ++ config.targetId), title config.hint ] [ Html.map never illu ]

                        _ ->
                            Html.map never illu

                Preset (Content content) ->
                    content

                None _ ->
                    Html.div [ css [ maxHeight (px 0), maxWidth (px 0) ] ] []

                CustomIllustration _ ->
                    Html.text "need to load custom illustration"

                CustomContent _ ->
                    Html.text "need to load custom content"
            ]
                |> Html.div [ class "body" ]

        additionalAttributes =
            s.additionalAttributes
                |> List.map (Attributes.map never)

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
                [ s.caption |> viewCaption |> View.notIf (hasBody s && mode.position.isLeaf && not mode.position.isRoot)
                , s.body |> viewBody
                , s.info |> View.ifJust (Tuple.second >> List.singleton >> Html.div [ class "info" ])
                , s.orientation |> orientationToString |> Html.text |> List.singleton |> View.overlay View.TopLeft |> View.debugOnly
                , mode.position.path |> List.map (Fold.viewDirection >> Html.text) |> View.overlay View.TopRight |> View.debugOnly
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
        Preset (Content _) ->
            "content"

        Preset (Illustration _) ->
            "illustration"

        CustomContent _ ->
            "content"

        CustomIllustration _ ->
            "illustration"

        None _ ->
            "noBody"


{-| -}
structureClass : Segment -> Html.Attribute msg
structureClass s =
    classList [ ( "noCaption", s.caption == [] ), ( "hasBody", hasBody s ) ]


{-| -}
hasBody : Segment -> Bool
hasBody s =
    case s.body of
        None _ ->
            False

        _ ->
            True
