module Snippets.Traces exposing (..)

import Accordion.Segment as Segment
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layout


foldernumber =
    "1MVHUG1PAKUmlCqeGuJSZQ0gMZV7Kb3hS"


traces : Segment.BodyTemplate
traces =
    div [ class "traces-container" ]
        [ iframe
            [ src <| "https://drive.google.com/embeddedfolderview?id=" ++ foldernumber ++ "#grid"
            , class "traces"
            ]
            []
        ]
        |> Segment.Illustration
