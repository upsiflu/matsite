module Ui.Layout exposing (Layout(..), view)

import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Keyed exposing (node, ul)
import List.Extra as List
import Ui.Get as Get exposing (Get)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel as ViewModel exposing (Foliage, ViewModel)


type Layout
    = Default


view : Layout -> ViewModel msg -> Foliage msg
view layout { handle, get } =
    case layout of
        Default ->
            ( "handle", node "nav" [ class "handle" ] handle )
                :: Get.toListBy niceLayout [ Scene, Control, Info ] get


niceLayout : Get (Foliage msg -> ( String, Html msg ))
niceLayout =
    Get.fromList
        [ ( Scene, ul [ class "scene" ] >> Tuple.pair "scene" )
        , ( Control, ul [ class "control" ] >> Tuple.pair "control" )
        , ( Info, ul [ class "info" ] >> Tuple.pair "info" )
        ]
