module Snippets.Anarchive exposing (anarchive, essays, incipit)

import Article
import Html.String as Html exposing (..)
import Html.String.Attributes as Attr exposing (class, href, src, title)
import Snippet exposing (cacheImg)


{-| -}
anarchive : Article.BodyTemplate
anarchive =
    Html.iframe
        [ Attr.attribute "width" "100%"
        , Attr.class "library"
        , Attr.style "height" "100%"
        , Attr.style "border" "0px"
        , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
        , title "Moving Across Thresholds - Library"
        ]
        []
        |> always
        |> Article.Content Nothing


{-| -}
incipit : Article.BodyTemplate
incipit =
    Html.div
        [ class "richtext"
        ]
        [ p [ class "meta" ]
            [ cacheImg "Library" 1 "" "https://lh6.googleusercontent.com/Ko6kFLF0ASL4bdqxLLK2t2Be-bIGY2B7YJzr5WC2c-9E3MK6IK08DuzQsbjC-nC-PrzrKOcxvYPeqOClmBYcWKVPEVvOrk-wqHoZxVaVcTLHHexkHOQHI6oZCGo7dbqq3GXRDVYf"
            , text """MaT's reference texts are available for everyone to read, download and/or add to. It's easy and simple to use. Please add texts with 'First name, Last name_Title'. Enjoy your digital wandering."""
            ]
        ]
        |> always
        |> Article.Illustration


{-| -}
essays : Article.BodyTemplate
essays =
    Html.div
        [ class "richtext"
        ]
        [ p []
            [ text """“Following cues from Walter Benjamin, I understand thresholds not so much as limits or borders [grenze], but as sites where potential swells [schwellen]. Think of wave energy – the metabolic pulse of the oceanic suck and push, where bodies and forces collide... Bodies and lived experiences are always threaded with thresholds: encounters that render us porous; chances to stitch ourselves anew. Thresholds, like waves, are generative zones of potential.”""" ]
        , p []
            [ text "--", a [ href "/ally-bisshop" ] [ text "Ally Bisshop" ] ]
        , p []
            [ a [ Attr.target "_blank", href "https://www.are.na/block/18079291" ]
                [ text "📖 Read Ally's essay 'For the exhausted ones: scores for moving through, moving with, fatigue.'" ]
            ]
        , hr [] []
        , p []
            [ text "“Creativity and innovation within soft limits is a general character of the MaT practice. Each session has a theme, a set of readings, proposed experiential practices and propositions, and often a co-facilitator. There is also a temporal limitation; the designated 90 minutes marks the boundaries of the event. This additionally makes the practice urgent, ephemeral and thus all the more precious.”" ]
        , p []
            [ text "--", a [ href "/balam-kenter" ] [ text "Balam Kenter" ] ]
        , p []
            [ a [ Attr.target "_blank", href "https://arena-attachments.s3.amazonaws.com/15227955/58dc7d974f3347933305b564051c6b09.pdf?1645189093" ]
                [ text "📖 Read Balam's essay in our library at are.na" ]
            ]
        ]
        |> always
        |> Article.Content (Just "Essays")
