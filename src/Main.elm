module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


import Data

main =
  Browser.sandbox 
    { init = 0
    , update = update
    , view = view >> Html.toUnstyled
    }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


view model =
    Html.div []
    [ Html.div 
        [ css 
            [ displayFlex
            , position relative
            , width (vw 100)
            , height (vh 100)
            , backgroundColor (rgb 100 200 200) ] 
        ]
        [ Data.anarchive ]
    , Html.div 
        [ css 
            [ displayFlex
            , position relative
            , width (vw 100)
            , height (vh 100)
            , backgroundColor (rgb 100 200 200) ] 
        ]
        [ Data.vimeo ]
    ]