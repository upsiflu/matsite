module Accordion.Segment exposing (Orientation(..), Segment, ViewMode(..), empty, preferMode, setPath, singleton, view, withBody, withOrientation)

{-| Segments only contain immutable data.
For the view, they can be overloaded with more information.
-}

import Css exposing (..)
import Fold exposing (Direction(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Layout exposing (..)
import Zipper.Tree as Tree exposing (Tree)


{-| -}
type alias Segment msg =
    { caption : Maybe String
    , id : String
    , body : Maybe (Html msg)
    , orientation : Orientation
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


singleton : String -> Segment msg
singleton id =
    { caption = Just id
    , id = String.replace " " "-" id
    , body = Nothing
    , orientation = Vertical
    }


empty : Segment msg
empty =
    { caption = Nothing
    , id = ""
    , body = Nothing
    , orientation = Vertical
    }



---- ViewModel ----


type alias Path =
    List Direction


{-| -}
setPath : Path -> ViewMode -> ViewMode
setPath p viewmode =
    case viewmode of
        Expanded config _ ->
            Expanded config p

        Collapsed _ ->
            Collapsed p

        Invisible ->
            Invisible


{-| -}
type ViewMode
    = Expanded { focused : Bool } Path
    | Collapsed Path
    | Invisible


{-| -}
preferMode : ViewMode -> ViewMode -> ViewMode
preferMode preference statusQuo =
    case ( preference, statusQuo ) of
        ( _, Invisible ) ->
            Invisible

        ( Invisible, _ ) ->
            Invisible

        ( Collapsed path0, Collapsed path1 ) ->
            Collapsed (path0 ++ path1)

        ( Collapsed path0, Expanded c path1 ) ->
            if c.focused then
                Expanded c (path0 ++ path1)

            else
                Collapsed (path0 ++ path1)

        ( Expanded c path0, Collapsed path1 ) ->
            if c.focused then
                Expanded c (path0 ++ path1)

            else
                Collapsed (path0 ++ path1)

        ( Expanded config path0, Expanded _ path1 ) ->
            Expanded config (path0 ++ path1)



---- View and helpers ----


{-| -}
view : ViewMode -> Segment msg -> Html msg
view mode s =
    let
        default =
            [ css
                [ overflowY scroll
                , Css.width (rem 21)
                , position relative
                ]
            ]

        functions =
            case mode of
                Expanded f path ->
                    if f.focused then
                        [ class "focused" ]

                    else
                        [ class "expanded" ]

                _ ->
                    [ class "collapsed" ]

        magic =
            case mode of
                Expanded _ _ ->
                    [ id s.id, css [ maxHeight (calc (vh 100) minus (rem 4)) ] ]

                Collapsed _ ->
                    [ id s.id, css [ maxHeight (rem 4) ] ]

                Invisible ->
                    [ css [ opacity (num 0.6), visibility Css.hidden ] ]

        directions =
            case mode of
                Expanded config path ->
                    path

                Collapsed path ->
                    path

                Invisible ->
                    []

        overlaid t =
            Html.div [ css [ position absolute, right zero, top zero, color (rgb 255 255 0), backgroundColor (rgb 0 0 100) ] ] [ Html.text t ]

        caption =
            case s.caption of
                Just h ->
                    header s.id h

                --Html.a [href ("#"++s.id)] [Html.text h]
                Nothing ->
                    Html.text "𐫱"
    in
    Html.div (default ++ functions ++ magic)
        [ caption
        , overlaid (List.map Fold.viewDirection directions |> String.join "")

        --, Html.text "Hoola"
        --, Html.text (List.length directions |> String.fromInt)
        , case s.body of
            Just b ->
                b

            Nothing ->
                Html.text ""
        ]
