module Snippets.Traces exposing (foldernumber, traces)

import Article
import Html.String exposing (..)
import Html.String.Attributes exposing (..)


foldernumber : String
foldernumber =
    "1MVHUG1PAKUmlCqeGuJSZQ0gMZV7Kb3hS"


traces : Article.BodyTemplate
traces =
    div [ class "traces-container" ]
        [ iframe
            [ src <| "https://drive.google.com/embeddedfolderview?id=" ++ foldernumber ++ "#grid"
            , class "traces"
            ]
            []
        ]
        |> always
        |> Article.Illustration
