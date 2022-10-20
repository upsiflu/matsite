module Snippets.Traces exposing (foldernumber, traces, tracesInfo)

import Article exposing (InfoTemplate(..))
import Html.String exposing (..)
import Html.String.Attributes exposing (..)
import Layout
import Snippet exposing (cacheImg)


foldernumber : String
foldernumber =
    "1MVHUG1PAKUmlCqeGuJSZQ0gMZV7Kb3hS"


tracesInfo : Article.InfoTemplate
tracesInfo =
    [ span [] [ text "MaT's collective documents are created by participants during events and growing with a temporality and life of their own. " ]
    , span []
        [ text "You can access them anytime and are welcome "
        , a [ target "_blank", href "https://drive.google.com/drive/folders/1MVHUG1PAKUmlCqeGuJSZQ0gMZV7Kb3hS?usp=sharing" ]
            [ text "to add, edit or create new ones" ]
        , text "."
        ]
    ]
        |> List.map Snippet.view
        |> Layout.bylineMulti Byline


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
