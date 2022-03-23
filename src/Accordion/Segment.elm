module Accordion.Segment exposing
    ( Segment
    , empty, singleton
    , Orientation(..)
    , withBody, withOrientation, withoutCaption, withAdditionalAttributes
    , view, structureClass, orientationToString, hasBody
    , decreaseColumnCount, increaseColumnCount, withAdditionalCaption
    )

{-| contain the immutable site content

_To render Segments differently based on their position in the tree, use
[`Segment.Viewmode`](Accordion.Segment.ViewMode)_

@docs Segment
@docs empty, singleton
@docs Orientation


# Map

@docs withBody, withOrientation, withoutCaption, withAdditionalAttributes


# View

@docs view, structureClass, orientationToString, hasBody

-}

import Accordion.Segment.ViewMode as ViewMode exposing (Role(..), ViewMode(..))
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
    , orientation : Orientation
    , columnCount : Int
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
    { segment | columnCount = segment.columnCount + 1 }


{-| -}
decreaseColumnCount : Segment msg -> Segment msg
decreaseColumnCount segment =
    { segment | columnCount = segment.columnCount - 1 }


{-| -}
withAdditionalAttributes : List (Html.Attribute Never) -> Segment msg -> Segment msg
withAdditionalAttributes cc segment =
    { segment | additionalAttributes = cc ++ segment.additionalAttributes }


{-| -}
singleton : String -> Segment msg
singleton id =
    { caption = [ id ]
    , id = String.replace " " "-" id
    , body = Nothing
    , orientation = Vertical
    , columnCount = 1
    , additionalAttributes = []
    }


{-| -}
empty : Segment msg
empty =
    { caption = []
    , id = "_"
    , body = Nothing
    , orientation = Vertical
    , columnCount = 1
    , additionalAttributes = []
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
            if mode == Placeholder then
                ""

            else
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
            Maybe.withDefault (Html.text "")

        additionalAttributes =
            s.additionalAttributes
                |> List.map (Attributes.map never)
    in
    Tuple.pair s.id <|
        case mode of
            Default { path, isLeaf } ->
                Html.li (ViewMode.toClass mode :: class "default" :: class (orientationToString s.orientation) :: id segmentId :: structureClass s :: additionalAttributes)
                    [ viewCaption s.caption |> notIf (s.body /= Nothing && isLeaf)
                    , viewOverlay (List.map Fold.viewDirection path |> String.join "") |> notIf (not debugging)
                    , viewBody s.body
                    , viewOrientation |> notIf (not debugging)
                    , ViewMode.view mode |> notIf (not debugging)
                    ]

            Collapsed { path, isLeaf } ->
                Html.li (ViewMode.toClass mode :: class "collapsed" :: class (orientationToString s.orientation) :: id segmentId :: structureClass s :: additionalAttributes)
                    [ viewCaption s.caption |> notIf (s.body /= Nothing && isLeaf)
                    , viewOverlay (List.map Fold.viewDirection path |> String.join "") |> notIf (not debugging)
                    , viewBody s.body
                    , viewOrientation |> notIf (not debugging)
                    , ViewMode.view mode |> notIf (not debugging)
                    ]

            Placeholder ->
                Html.li (ViewMode.toClass mode :: class "placeholder" :: class (orientationToString s.orientation) :: id segmentId :: structureClass s :: additionalAttributes)
                    [ viewCaption s.caption
                    , viewBody s.body
                    , ViewMode.view mode |> notIf (not debugging)
                    ]


{-| -}
orientationToString orientation =
    case orientation of
        Horizontal ->
            "🀱"

        Vertical ->
            "🁣"


{-| -}
structureClass s =
    classList [ ( "noCaption", s.caption == [] ), ( "hasBody", hasBody s ) ]


{-| -}
hasBody : Segment a -> Bool
hasBody =
    .body >> (/=) Nothing
