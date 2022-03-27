module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withBody, withOrientation, withoutCaption, withAdditionalCaption, withInfo, withAdditionalAttributes
    , decreaseColumnCount, increaseColumnCount
    , view, structureClass, orientationToString, hasBody
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

@docs withBody, withOrientation, withoutCaption, withAdditionalCaption, withInfo, withAdditionalAttributes
@docs decreaseColumnCount, increaseColumnCount


# View

@docs view, structureClass, orientationToString, hasBody

-}

import Accordion.Segment.ViewMode as ViewMode exposing (ViewMode, Width(..))
import Css exposing (..)
import Fold exposing (Direction(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Layout exposing (..)
import Zipper.Tree as Tree exposing (Tree)


debugging =
    False


{-| -}
type alias Segment msg =
    { caption : List String
    , id : String
    , body : Maybe (Html msg)
    , info : Maybe (Html msg)
    , orientation : Orientation
    , width : Width
    , additionalAttributes : List (Html.Attribute Never)
    }


{-| -}
type Orientation
    = Vertical
    | Horizontal


{-| -}
withOrientation : Orientation -> Segment msg -> Segment msg
withOrientation orientation segment =
    { segment | orientation = orientation }


{-| -}
withBody : Html msg -> Segment msg -> Segment msg
withBody body segment =
    { segment | body = Just body }


{-| -}
withInfo : Html msg -> Segment msg -> Segment msg
withInfo info segment =
    { segment | info = Just info }


{-| -}
withoutCaption : Segment msg -> Segment msg
withoutCaption segment =
    { segment | caption = [] }


{-| -}
withAdditionalCaption : String -> Segment msg -> Segment msg
withAdditionalCaption string segment =
    { segment | caption = string :: segment.caption }


{-| -}
increaseColumnCount : Segment msg -> Segment msg
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
decreaseColumnCount : Segment msg -> Segment msg
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
withAdditionalAttributes : List (Html.Attribute Never) -> Segment msg -> Segment msg
withAdditionalAttributes cc segment =
    { segment | additionalAttributes = cc ++ segment.additionalAttributes }


{-| -}
singleton : String -> Segment msg
singleton id =
    { empty
        | caption = [ id ]
        , id = String.replace " " "-" id
    }


{-| -}
empty : Segment msg
empty =
    { caption = []
    , id = "_"
    , body = Nothing
    , orientation = Vertical
    , width = Columns 1
    , additionalAttributes = []
    , info = Nothing
    }



---- View and helpers ----


{-| -}
view : ViewMode -> Segment msg -> ( String, Html msg )
view mode s =
    let
        viewOrientation =
            Html.div [ css [ position absolute, left zero, top zero ] ]
                [ Html.text (orientationToString s.orientation) ]

        ----
        viewOverlay =
            Html.text
                >> List.singleton
                >> Html.div
                    [ css [ position absolute, right zero, top zero, color (rgb 255 255 0), backgroundColor (rgba 255 255 0 0.1), Css.property "writing-mode" "horizontal-tb" ] ]

        segmentId =
            s.id

        viewCaption cc =
            case cc of
                [] ->
                    viewCaption [ "⋮" ]

                [ one ] ->
                    header "" segmentId one

                _ ->
                    List.map (header "" segmentId) cc
                        |> Html.div [ class "multipleHeaders", css [ displayFlex, justifyContent spaceBetween ] ]

        notIf bool =
            if bool then
                \_ -> Html.text ""

            else
                identity

        viewBody =
            Maybe.withDefault (Html.div [ css [ maxHeight (px 0), maxWidth (px 0) ] ] [ Html.text "" ])

        additionalAttributes =
            s.additionalAttributes
                |> List.map (Attributes.map never)

        viewInfo =
            case s.info of
                Nothing ->
                    Html.text ""

                Just info ->
                    Html.div [ class "info" ] [ info ]

        { path, isLeaf, isRoot } =
            mode.position
    in
    Tuple.pair s.id <|
        Html.li (ViewMode.toClass mode :: class (orientationToString s.orientation) :: id segmentId :: structureClass s :: additionalAttributes)
            [ viewCaption s.caption |> notIf (s.body /= Nothing && isLeaf && not isRoot)
            , viewOverlay (List.map Fold.viewDirection path |> String.join "") |> notIf (not debugging)
            , viewBody s.body
            , viewInfo
            , viewOrientation |> notIf (not debugging)
            ]


{-| -}
orientationToString : Orientation -> String
orientationToString orientation =
    case orientation of
        Horizontal ->
            "🀱"

        Vertical ->
            "🁣"


{-| -}
structureClass : Segment msg -> Html.Attribute msg
structureClass s =
    classList [ ( "noCaption", s.caption == [] ), ( "hasBody", hasBody s ) ]


{-| -}
hasBody : Segment a -> Bool
hasBody =
    .body >> (/=) Nothing
