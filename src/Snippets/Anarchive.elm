module Snippets.Anarchive exposing (..)

import Accordion.Segment as Segment
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)


{-| -}
anarchive : Segment.BodyTemplate
anarchive =
    Html.div [ Attr.class "anArchive" ]
        [ Html.iframe
            [ Attr.attribute "width" "100%"
            , css [ position absolute, Css.height (pct 100), border (px 0) ]
            , Attr.src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
            , Attr.title "Moving Across Thresholds - AnArchive"
            ]
            []
        ]
        |> Segment.Content
