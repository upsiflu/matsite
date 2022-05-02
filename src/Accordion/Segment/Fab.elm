module Accordion.Segment.Fab exposing (Fab(..))

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (..)
import Occurrence exposing (Occurrence)


type Fab
    = Register { link : String, occurrence : Occurrence }
    | Subscribe { link : String }


edit : { save : Fab -> msg } -> Fab -> Html msg
edit { save } fab =
    Html.text "TODO Fab.edit"


view : Fab -> Html Never
view fab =
    case fab of
        Register { link } ->
            Html.a [ class "register fab", href link ] [ Html.span [] [ Html.text "register" ] ]

        Subscribe { link } ->
            Html.a [ class "subscribe fab", href link ] [ Html.span [] [ Html.text "register" ] ]
