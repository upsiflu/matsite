module Snippets.Artist exposing (..)

import Accordion.Segment as Segment
import Html.Styled as Html exposing (Html, a, div, p, text)
import Html.Styled.Attributes exposing (..)
import List.Extra as List


type alias Artist =
    { name : String
    , bio : Html Never
    , photo : String
    , wide : Bool
    }


artists : List Artist
artists =
    [ { name = "Anna Mayberry"
      , bio = Html.p [] [ Html.text "is an English-French architect and writer living in Berlin. In 2017, she completed an M.Sc. in Architecture (ETH Zürich). Her thesis explored the hidden backstage spaces in Zürich’s historical center and how small-scale architectural interventions could enhance these public spaces for city-dwellers. Since 2018 she has been working for the architecture office Bauereignis, a Berlin-based team which focuses on “participatory design and construction projects for the development of schools” ", Html.a [ target "_blank", href "www.bauereignis.de" ] [ Html.text "(www.bauereignis.de)" ], Html.text ". Anna Mayberry has an ongoing writing and drawing practice where she documents the stories of the places and faces she encounters in Berlin or along her travels. This exploration is a source of inspiration in her architectural designs, where she seeks to create built environments which make space for the rhythm of everyday life." ]
      , photo = "asset/photo/Anna Mayberry.jpg"
      , wide = False
      }
    , { name = "Balam Kenter"
      , bio = Html.p [] [ Html.text "Balam is three cats in a trench coat who intermittently passes for a PhD student at the Centre for Interdisciplinary Studies in Society and Culture at Concordia University, Tio’Tia:Ke. Balam has always been interested in bodies, especially non-normative ones, especially at the point of encounter with oppressive systems. Their current academic work is situated at the intersections of Political Philosophy, Critical Disability Studies, and Critical Animal Studies. Having explored the relationship between exploitation and oppression through disability at Boğaziçi University’s Philosophy MA program, they are now focusing on the political, historical, and material entanglements of ableism and anthropocentrism under late capitalism. When they are not studying in Montreal, Balam lives with two humans, three cats, and a dog in Istanbul. Unless they are on a strict deadline, Balam can often be found (or lost) researching tangential topics, watching horror films, and doodling. ", Html.a [ class "weblink", target "_blank", href "https://balamkenter.com/" ] [ text "  https://balamkenter.com/" ] ]
      , photo = "asset/photo/balam1.jpg"
      , wide = False
      }
    , { name = "Julia Grillmayr"
      , bio = Html.p [] [ Html.text "Julia Grillmayr is a Vienna- and Linz-based cultural studies scholar, journalist, radio maker and science communicator. She holds a PhD in Comparative Literature from the University of Vienna. At the University of Art and Design Linz, she explored the relationship between contemporary SF literature and futurological scenarios. Her research interests are speculative (eco)feminist philosophy and cyberpunk. She is creator of the radio broadcast Superscience Me on Radio Orange, she works for the public radio channel Ö1, and podcasts for the Austrian Academy of Science. The rest of the time, she spends in muddy danubian wetlands and in tap shoes.", Html.a [ class "weblink", target "_blank", href "https://juliagrillmayr.at/" ] [ Html.text "https://juliagrillmayr.at/" ] ]
      , photo = "asset/photo/Portrait_Grillmayr.jpg"
      , wide = False
      }
    , { name = "Rivca Rubin"
      , bio = Html.p [] [ Html.text "Rivca is described as a catalyst of fresh liberating approaches to shifting oppressive systems and co-creating those beneficial to all people with consciously considered communicating at the heart of all relating and egalitarian engaging. Rivca works with leadership, artists and activists in the cultural, educational, business and non-governmental sectors in Europe and worldwide, such as the Clore Leadership Programme, Unesco Literature Fund, BBC Academy, BFI and Regional Screen Archives, Manchester Museum, Manchester and Whitworth Art Galleries, Invisible Dust, Global Grooves Future Leaders, VANZA Leadership Programme, All Out Africa, SEEDCommunity South Africa, PAP, TAK and HZT Berlin, HfMDK Frankfurt, Akademie for Performing Arts Producer, Germany. Rivca presents at international conferences and networks, is a co-initiator of GrandReUnion, Tender Hotel, and joint director of Islington Mill, Salford, Manchester.", Html.a [ class "weblink", target "_blank", href "http://www.rivcarubin.com" ] [ Html.text "http://www.rivcarubin.com" ], Html.a [ class "weblink", target "_blank", href "http://www.upwording.com" ] [ Html.text "http://www.upwording.com" ] ]
      , photo = "asset/photo/Rivca_Credit_ Roshana Rubin-Mayhew.jpeg"
      , wide = False
      }
    , { name = "Ally Bisshop"
      , bio = Html.p [] [ Html.text "Ally Bisshop is an Australian artist, writer, and researcher living in Berlin. She draws on methods and concepts across multiple disciplines to think critically and creatively about the material, affective, ethical, and relational thresholds between human and nonhuman. In 2018 Ally completed a Ph.D. in visual arts (UNSW Sydney's National Institute for Experimental Arts) with an exploration of more-than-human techniques in processual artistic praxis. Previously, she studied art at the UdK Berlin through Olafur Eliasson’s Institut für Räumexperimente, and microbiology (B.Sc. Hons 1) at UQ Brisbane. Since 2017 she has been an associate researcher in Berlin-based artist Tomás Saraceno’s transdisciplinary interspecies Arachnophilia project.", Html.a [ class "weblink", href "https://allybisshop.com/" ] [ Html.text "https://allybisshop.com/" ], Html.i [] [ Html.text "Link to Fatigue as creative proposition" ] ]
      , photo = "asset/photo/Ally Bisshop.png"
      , wide = True
      }
    ]
        |> List.sortBy (.name >> String.split " " >> List.last >> Maybe.withDefault "z")


toc : (String -> Html.Attribute msg) -> Segment.Info
toc generateLink =
    artists
        |> List.map .name
        |> List.map
            (\name -> Html.li [] [ Html.a [ target "_self", generateLink name ] [ Html.text name ] ])
        |> Html.span [ class "toc" ]
        |> Segment.Toc


view : Artist -> Segment.Body
view artist =
    Html.div [ class "artist richtext" ]
        [ Html.h2 [] [ Html.text artist.name ]
        , Html.map never artist.bio
        ]
        |> Segment.Content


viewPhoto : Artist -> Segment.Body
viewPhoto artist =
    Html.img [ class "artist", src artist.photo ] [] |> Segment.Illustration
