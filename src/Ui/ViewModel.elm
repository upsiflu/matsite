module Ui.ViewModel exposing (ViewModel, concat, concatMap, cons, view, void)

import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Keyed exposing (ul)
import List.Extra as List
import Ui.Layout as Layout exposing (Layout)


{-| Populate four regions
-}
type alias ViewModel msg =
    { handles : List (Html msg)
    , scenes : List ( String, Html msg )
    , infos : List ( String, Html msg )
    , controls : List ( String, Html msg )
    }


{-| Empty `ViewModel`
-}
void : ViewModel msg
void =
    ViewModel [] [] [] []


{-| combine two ViewModels into one
-}
cons : ViewModel msg -> ViewModel msg -> ViewModel msg
cons a acc =
    { acc
        | handles = a.handles ++ acc.handles
        , scenes = a.scenes ++ acc.scenes
        , infos = a.infos ++ acc.infos
        , controls = a.controls ++ acc.controls
    }


{-| Compose `ViewModels` orthogonally: without mutual influence.
-}
concat : List (ViewModel msg) -> ViewModel msg
concat =
    List.foldl1 cons
        >> Maybe.withDefault void


{-| Generate a list of ViewModels, then `concat` it
-}
concatMap : (a -> ViewModel msg) -> List a -> ViewModel msg
concatMap fu list =
    List.map fu list |> concat


view : Layout -> ViewModel msg -> List ( String, Html msg )
view layout { handles, scenes, infos, controls } =
    case layout of
        Layout.Default ->
            [ ( "handle", Html.div [ class "handle" ] handles )
            , ( "scene", ul [ class "scene" ] scenes )
            , ( "info", ul [ class "info" ] infos )
            , ( "control", ul [ class "control" ] controls )
            ]
