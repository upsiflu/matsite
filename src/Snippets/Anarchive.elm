module Snippets.Anarchive exposing (..)

import Accordion.Article as Article
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (class, css, href, src, title)


{-| -}
anarchive : Article.BodyTemplate
anarchive =
    Html.iframe
        [ Attr.attribute "width" "100%"
        , Attr.class "library"
        , css [ Css.height (pct 100), border (px 0) ]
        , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
        , title "Moving Across Thresholds - Library"
        ]
        []
        |> Article.Content Nothing


{-| -}
incipit : Article.BodyTemplate
incipit =
    Html.div
        [ class "richtext"
        ]
        [ img [ src "https://lh6.googleusercontent.com/Ko6kFLF0ASL4bdqxLLK2t2Be-bIGY2B7YJzr5WC2c-9E3MK6IK08DuzQsbjC-nC-PrzrKOcxvYPeqOClmBYcWKVPEVvOrk-wqHoZxVaVcTLHHexkHOQHI6oZCGo7dbqq3GXRDVYf" ] []
        , p []
            [ text "“Creativity and innovation within soft limits is a general character of the MaT practice. Each session has a theme, a set of readings, proposed experiential practices and propositions, and often a co-facilitator. There is also a temporal limitation; the designated 90 minutes marks the boundaries of the event. This additionally makes the practice urgent, ephemeral and thus all the more precious.”" ]
        , p []
            [ text "--", a [ href "/balam-kenter" ] [ text "Balam Kenter" ] ]
        ]
        |> Article.Illustration
