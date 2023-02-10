module Snippets.Anarchive exposing (Essay, anarchive, blurb, essay0, essay1, essays, incipit, viewEssay)

import Article exposing (InfoTemplate(..))
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
        |> Article.Content (Just "Worded Companions")


{-| -}
incipit : Article.BodyTemplate
incipit =
    Html.div
        [ class "richtext"
        ]
        [ p [ class "meta" ]
            [ cacheImg "Library" 1 "" "https://lh6.googleusercontent.com/Ko6kFLF0ASL4bdqxLLK2t2Be-bIGY2B7YJzr5WC2c-9E3MK6IK08DuzQsbjC-nC-PrzrKOcxvYPeqOClmBYcWKVPEVvOrk-wqHoZxVaVcTLHHexkHOQHI6oZCGo7dbqq3GXRDVYf"
            , text """MaT's commissioned essays and reference texts are available for everyone to read, download and/or add to. It's easy and simple to use. Please add texts with 'First name, Last name_Title'. Enjoy your digital wandering."""
            ]
        ]
        |> always
        |> Article.Illustration


{-| -}
essay0 : Article.BodyTemplate
essay0 =
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
        ]
        |> always
        |> Article.Content (Just "For the exhausted ones")


{-| -}
essay1 : Article.BodyTemplate
essay1 =
    Html.div
        [ class "richtext"
        ]
        [ p []
            [ text "“Creativity and innovation within soft limits is a general character of the MaT practice. Each session has a theme, a set of readings, proposed experiential practices and propositions, and often a co-facilitator. There is also a temporal limitation; the designated 90 minutes marks the boundaries of the event. This additionally makes the practice urgent, ephemeral and thus all the more precious.”" ]
        , p []
            [ text "--", a [ href "/balam-kenter" ] [ text "Balam Kenter" ] ]
        , p []
            [ a [ Attr.target "_blank", href "https://arena-attachments.s3.amazonaws.com/15227955/58dc7d974f3347933305b564051c6b09.pdf?1645189093" ]
                [ text "📖 Read Balam's essay 'Hello, good evening, and welcome to The Middle of the Film!'" ]
            ]
        ]
        |> always
        |> Article.Content (Just "Welcome to The Middle of the Film!")


type alias Essay =
    { heading : String
    , year : Int
    , author : String
    , excerpt : String
    , link : String
    }


essays : List Essay
essays =
    [ { heading = "'Structure is magic. Architecture, policy, and design.'"
      , year = 2022
      , author = "Ssempijja Robert"
      , excerpt = "Ugandan contemporary artist and dance researcher, Robert Ssempijja, works in traditional and non-traditional spaces in an era of post-colonialism and decolonization. Robert's story unfolds in the gaps between Kololo and Kiyayye, both mentally and physically."
      , link = "https://www.are.na/block/20284898"
      }
    , { heading = "'Weaving encounters within thresholds' \n"
      , year = 2022
      , author = "Giovana de Souza Possignolo\n"
      , excerpt = "Giovana de Souza Possignolo develops participatory action research with peripheral women within urban borders of Brazil. As a facilitator of ‘Moving across Thresholds’, Giovana together with the cultural community Quilombaque in Perus, Brazil invited all participants based in Berlin to think-feel what center-periphery means, de-centralising Western concepts of being and relating with others and spaces.\n"
      , link = "https://www.are.na/block/20285168\n"
      }
    , { heading = "'Sound outside of sound'\n"
      , year = 2022
      , author = "Samuel Hertz "
      , excerpt = "Sound is a fuzzy transport allowing passage between borders presumed solid. Berlin-based composer and researcher, Samuel Hertz, explores connections between sound and climate, emphasising geographical listening practices at more-than-human scales.\n"
      , link = "https://www.are.na/block/20285387\n"
      }
    , { heading = "'Marx and Foucault Go to the Movies: Horror as Method in Understanding Anthropocentrism and Ableism' \n"
      , year = 2022
      , author = " Balam Kenter"
      , excerpt = "Balam Kenter, a PhD candidate at the Centre for Interdisciplinary Studies in Society and Culture at Concordia University, focuses on the political, historical, and material entanglements of ableism and anthropocentrism under late capitalism. \n"
      , link = "https://www.are.na/block/20285631\n"
      }
    , { heading = "‘I think this may be a threshold’ \n"
      , year = 2022
      , author = "Anna Farley \n"
      , excerpt = "The central mixing pot and existing in it is the practice of thresholding for Anna Farley, an Autistic artist focused on access and inclusion with regards to neuro-difference and disability. Anna’s training, consultation and lived experience creates the basis for her art works. \n"
      , link = "https://www.are.na/block/20286441\n"
      }
    , { heading = "'For the exhausted ones: scores for moving through, moving with, fatigue.' \n"
      , year = 2022
      , author = "Ally Bisshop\n"
      , excerpt = "Ally Bisshop is a transdisciplinary artistic researcher whose work critically and creatively explores the material, affective, ethical and relational thresholds between human and nonhuman. Ally understands thresholds not so much as limits or borders [grenze], but as sites where potential swells [schwellen].\n"
      , link = "https://www.are.na/block/18079291\n"
      }
    , { heading = "'Hello, good evening, and welcome to The Middle of the Film!' \n"
      , year = 2021
      , author = "Balam Kenter\n"
      , excerpt = "Creativity and innovation within soft limits is a general character of the MaT practice. In this essay Balam Kenter, a PhD candidate at the Centre for Interdisciplinary Studies in Society and Culture at Concordia University, reflects on the MaT’s emerging methods between 2020-21, Series 1-3.\n"
      , link = "https://www.are.na/block/15227955\n"
      }
    ]


viewEssay : Essay -> Article.BodyTemplate
viewEssay essay =
    (\dir ->
        Html.div
            [ class "richtext"
            ]
            [ p []
                [ text ("Commissioned by MaT, " ++ String.fromInt essay.year) ]
            , p []
                [ text essay.excerpt ]
            , p []
                [ text "by ", a [ href ("/" ++ essay.author) ] [ text essay.author ] ]
            , p []
                [ a [ Attr.target "_blank", href essay.link ]
                    [ text "📖 Read the full essay" ]
                ]
            ]
    )
        |> Article.Content (Just essay.heading)


blurb : InfoTemplate
blurb =
    Byline 1 (Snippet.view <| Html.text "MaT's commissioned essays and reference texts are available for everyone to read, download and/or add to. It's easy and simple to use. Please add texts with 'First name, Last name_Title'. Enjoy your digital wandering.")
