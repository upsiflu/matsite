module Ui.Layout exposing (Layout(..))

import Html.Styled as Html exposing (Attribute, Html, details, div, input, label, span, summary)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Keyed exposing (ul)
import List.Extra as List


type Layout
    = Default
