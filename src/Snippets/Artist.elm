module Snippets.Artist exposing (Artist, artists, view, viewPhoto)

import Article
import Directory exposing (Directory)
import Html.String as Html exposing (Html, text)
import Html.String.Attributes exposing (..)
import Layout
import List.Extra as List
import Snippet exposing (cacheImg)


type alias Artist =
    { name : String
    , bio : Directory -> Html Never
    , photo : String
    , wide : Bool
    }


artists : List Artist
artists =
    let
        event dir descr fblink =
            let
                closeEvent =
                    Directory.getClosestBy (String.length descr // 4) descr dir
                        |> Maybe.map
                            (\uuid -> Html.a [ href uuid ] [ Html.text "☛ Moving across Thresholds Lab" ])
                        |> Maybe.withDefault Snippet.none
            in
            Html.fieldset []
                [ Html.legend [] [ Html.text (descr ++ ":") ]
                , closeEvent
                , Html.a [ class "weblink", target "_blank", href fblink ] [ Html.text " [fb event page]" ]
                ]

        festivalEvent dir descr =
            Directory.getClosestBy (String.length descr // 4) descr dir
                |> Maybe.map
                    (\uuid -> Html.a [ href uuid ] [ Html.text "☛ Festival" ])
                |> Maybe.map
                    (\closeEvent ->
                        Html.fieldset []
                            [ Html.legend [] [ Html.text (descr ++ ":") ]
                            , closeEvent
                            ]
                    )
                |> Maybe.withDefault Snippet.none

        festival descr location outlink =
            Html.a [ class "weblink", target "_blank", href outlink ] [ Html.text (descr ++ " [" ++ location ++ "]") ]

        weblink descr destination =
            Html.a [ class "weblink", target "_blank", href destination ] [ Html.text descr ]

        internal dir descr =
            Directory.getClosestBy (String.length descr // 4) descr dir
                |> Maybe.map
                    (\uuid -> Html.a [ class "internal", href (Layout.sanitise uuid) ] [ Html.text (" ☛" ++ descr ++ " ") ])
                |> Maybe.withDefault (text descr)

        --Directory.maybeLink descr dir
        --Html.a [ class "internal", href ("/" ++ Layout.sanitise descr) ] [ Html.text (" ☛" ++ descr ++ " ") ]
    in
    [ { name = "Anna\u{00A0}Mayberry"
      , bio = \dir -> Html.p [] [ Html.text "is an English-French architect and writer living in Berlin. In 2017, she completed an M.Sc. in Architecture (ETH Zürich). Her thesis explored the hidden backstage spaces in Zürich’s historical center and how small-scale architectural interventions could enhance these public spaces for city-dwellers. Since 2018 she has been working for the architecture office Bauereignis, a Berlin-based team which focuses on “participatory design and construction projects for the development of schools” ", Html.a [ target "_blank", href "https://bauereignis.de" ] [ Html.text "(https://bauereignis.de)" ], Html.text ". Anna Mayberry has an ongoing writing and drawing practice where she documents the stories of the places and faces she encounters in Berlin or along her travels. This exploration is a source of inspiration in her architectural designs, where she seeks to create built environments which make space for the rhythm of everyday life." ]
      , photo = "https://lh3.googleusercontent.com/x80L4cgraRGQsq0juS4izJwhwAMsvKE-Hu3sLXk6UsnjKf3mz-evo7J4espwIk1zep5BEZ-oxobH099Msoi68N4-W9IqyObHlpcYqh759JRNGReInZY50arDy6QL3jvT5-91xHFugugv7hNKog"
      , wide = False
      }
    , { name = "Balam\u{00A0}Kenter"
      , bio = \dir -> Html.p [] [ Html.text "Balam is three cats in a trench coat who intermittently passes for a PhD student at the Centre for Interdisciplinary Studies in Society and Culture at Concordia University, Tio’Tia:Ke. Balam has always been interested in bodies, especially non-normative ones, especially at the point of encounter with oppressive systems. Their current academic work is situated at the intersections of Political Philosophy, Critical Disability Studies, and Critical Animal Studies. Having explored the relationship between exploitation and oppression through disability at Boğaziçi University’s Philosophy MA program, they are now focusing on the political, historical, and material entanglements of ableism and anthropocentrism under late capitalism. When they are not studying in Montreal, Balam lives with two humans, three cats, and a dog in Istanbul. Unless they are on a strict deadline, Balam can often be found (or lost) researching tangential topics, watching horror films, and doodling. ", Html.a [ class "weblink", target "_blank", href "https://balamkenter.com/" ] [ text "  https://balamkenter.com/" ] ]
      , photo = "https://lh3.googleusercontent.com/am3kGTLYql5xaKoh-A9f3W2IR7eUws_FZQeLJ1CuzAvaPg06aFIDWpq2VaAzfiw8KgMNmrbfhqKUgHC92CcyOdH9rZnXrpk7nLVpaCQSGHZwvcxbymxgDSzd-mUojWoaC0tfMGcwVMkfRE3eHA"
      , wide = False
      }
    , { name = "Julia\u{00A0}Grillmayr"
      , bio = \dir -> Html.p [] [ Html.text "Julia Grillmayr is a Vienna- and Linz-based cultural studies scholar, journalist, radio maker and science communicator. She holds a PhD in Comparative Literature from the University of Vienna. At the University of Art and Design Linz, she explored the relationship between contemporary SF literature and futurological scenarios. Her research interests are speculative (eco)feminist philosophy and cyberpunk. She is creator of the radio broadcast Superscience Me on Radio Orange, she works for the public radio channel Ö1, and podcasts for the Austrian Academy of Science. The rest of the time, she spends in muddy danubian wetlands and in tap shoes.", Html.a [ class "weblink", target "_blank", href "https://juliagrillmayr.at/" ] [ Html.text "https://juliagrillmayr.at/" ] ]
      , photo = "https://lh4.googleusercontent.com/7LLFpU2L0El8Dg9mRS6zZb4kG1bE9-PyAfrcuZ_cBoOZ3AytTW-7LxX0srwjh44wo6et2BYjSBnTTSigvMr4XZPujU_UOEX4r7f0Owgm1UE7VIgCwYTWVOaKPvm8B6kLx2BlWZ_chtYj03vnvA"
      , wide = False
      }
    , { name = "Rivca\u{00A0}Rubin"
      , bio = \dir -> Html.p [] [ Html.text "Rivca is described as a catalyst of fresh liberating approaches to shifting oppressive systems and co-creating those beneficial to all people with consciously considered communicating at the heart of all relating and egalitarian engaging. Rivca works with leadership, artists and activists in the cultural, educational, business and non-governmental sectors in Europe and worldwide, such as the Clore Leadership Programme, Unesco Literature Fund, BBC Academy, BFI and Regional Screen Archives, Manchester Museum, Manchester and Whitworth Art Galleries, Invisible Dust, Global Grooves Future Leaders, VANZA Leadership Programme, All Out Africa, SEEDCommunity South Africa, PAP, TAK and HZT Berlin, HfMDK Frankfurt, Akademie for Performing Arts Producer, Germany. Rivca presents at international conferences and networks, is a co-initiator of GrandReUnion, Tender Hotel, and joint director of Islington Mill, Salford, Manchester.", Html.a [ class "weblink", target "_blank", href "http://www.rivcarubin.com" ] [ Html.text "http://www.rivcarubin.com" ], Html.a [ class "weblink", target "_blank", href "http://www.upwording.com" ] [ Html.text "http://www.upwording.com" ] ]
      , photo = "https://lh4.googleusercontent.com/1R5eaNVfVJytf892zZGbR82TJTUv30_9vHmHu4DXqa4G4ZXBJ_uFQF_Puf5Mbl5aa9VjttHLbXflARqOYz7NY_euQrttTw5tkYh6UTfzDM1a9ckl_FVwmBZJ16JTdG6sszSUtTIGcE_y3OlXmg"
      , wide = False
      }
    , { name = "Ally\u{00A0}Bisshop"
      , bio = \dir -> Html.p [] [ Html.text "is an Australian artist, writer, and researcher living in Berlin. She draws on methods and concepts across multiple disciplines to think critically and creatively about the material, affective, ethical, and relational thresholds between human and nonhuman. In 2018 Ally completed a Ph.D. in visual arts (UNSW Sydney's National Institute for Experimental Arts) with an exploration of more-than-human techniques in processual artistic praxis. Previously, she studied art at the UdK Berlin through Olafur Eliasson’s Institut für Räumexperimente, and microbiology (B.Sc. Hons 1) at UQ Brisbane. Since 2017 she has been an associate researcher in Berlin-based artist Tomás Saraceno’s transdisciplinary interspecies Arachnophilia project.", Html.a [ class "weblink", href "https://allybisshop.com/" ] [ Html.text "https://allybisshop.com/" ], Html.i [] [ Html.text "Link to Fatigue as creative proposition" ] ]
      , photo = "https://lh6.googleusercontent.com/kJjIIvJeffNdqTIFCGaANwI6xY85zqTQTJf4XdJ4PWikurkXsOlkZBehEUD7Zfi1V70e7zYOiCA9Rt3fS0L5PNEHsCOU4LFrpRPNSwiYlKY89DrqXHWWUfE967k9GIYHVp7YxNKx2Nycggx_jw"
      , wide = True
      }
    , { name = "Nattan Dobkin"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "based both in Berlin and Tel aviv, is a performance artist, a teacher, and an adjunct professor in the department of gender studies at Ben Gurion University of the Negev, Be’er Sheva, for which he/they had been noted an outstanding lecturer. He/They teach a performance-art course at the Pre-Academic Art School “The Garage”, for students facing mental health challenges. Dobkin regularly facilitates and independant workshop in Tel Aviv that deals with the performer’s identity in gender and sexual contexts, and in the stage world. Dobkin holds a M.A in gender studies from Ben Gurion University of the Negev, Be’er Sheva, and a B.A in theatre directing and pedagogy from Seminar Hakibutzim College, Tel Aviv. "
                    ]
      , photo = "https://lh3.googleusercontent.com/qqY_NjTLxinp1hkfnhYT5Dh6mEldDluVUssrW2mAhxD-HnRIyp2t5o-J4uiKU1hmwZw1hPJwN0cjNiOv3uPCtnCfbtdzmyRSTdRmhBgnuty4tgIZzkWEQf33iTHlMQQpo2oLTKaWysSNmYiW8Q"
      , wide = False
      }
    , { name = "Erin\u{00A0}Manning"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "Erin Manning is a professor in the Faculty of Fine Arts at Concordia University (Montreal, Canada). She is also the founder of SenseLab (www.senselab.ca), a laboratory that explores the intersections between art practice and philosophy through the matrix of the sensing body in movement. Current art projects are focused around the concept of minor gestures in relation to colour and movement. Publications include The Minor Gesture (Duke UP, 2016), Always More Than One: Individuation’s Dance (Duke UP, 2013), Relationscapes: Movement, Art and Philosophy (Cambridge, Mass.: MIT Press, 2009), among others. "
                    , weblink "erinmovement.com" "http://erinmovement.com/"
                    , event dir "Erin Manning Conversation" "https://www.facebook.com/events/1076686642751677/"
                    ]
      , photo = "https://lh5.googleusercontent.com/IxDNp_X7e5mCW7hFWi7JmB3YkfUxPf41sjREB-Yc4yruUeK5zWA3Jey6BhvUpix6MQxnXlfu68aEk-u2vwfih_zVcau2LnBhba4n24W4s4AU6xN32Vn7pk-XD6KO0D2FSATZ47-d0vDhvSl-8Q"
      , wide = True
      }
    , { name = "Mmakgosi\u{00A0}Kgabi"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a trained physical theatre and improvisation performer and also a performance facilitator. She has developed her skills in the industry, both in South Africa and internationally as an on Screen Actor and a Stage Actor. Her work often interrogates the premise of identity and nationalism, revisiting themes on the Black Female Body Politics and Migration. She is a firm believer in the power of performance to build a better world.  "
                    , weblink "mmakgosikgabi.com" "https://www.mmakgosikgabi.com/"
                    , event dir "Dances of the Mouth" "https://www.facebook.com/events/377970686802376/"
                    ]
      , photo = "https://lh5.googleusercontent.com/ZJxm0WNN737-p-pHXWhKauYm-HwDyY6TFIx6cZfi1nHTiyglebUM_lipe8IKfC6EuDWCgruc2_yCmBeVSjAtKy25UUZVOxCiqdEd0PHYUXbWTvGUNgYyWJOhUP_iQM4mT8XaqnvaI6APhfUFGw"
      , wide = False
      }
    , { name = "Sindri\u{00A0}Runudde"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a Swedish and internationally based dancer and choreographer. Sindri’s work spans between different fields that include sound art, visual art and performance. Previously educated in contemporary circus and dance, Sindri works from a queer feminist perspective and from lived experience. They are involved internationally in the disabled/crip dance community."
                    , event dir "Dances of Care" "https://www.facebook.com/events/206132577660551/"
                    ]
      , photo = "https://lh3.googleusercontent.com/FsqQn9QBmI5LkcJZRFmNFtzJvLFhnh_1h0kuSTAFsjhSrZOola6jafd41GFLcywIoxjnSlsihjXzZkIa9jH-4aIR5bWEgKYSt2Jj_wYTYZrprbYhDbqGE_I3QZlsfLPWGGcqXk60DEt57UtRJQ"
      , wide = False
      }
    , { name = "Susanne\u{00A0}Schmitt"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is an anthropologist and sensory ethnographer, interdisciplinary artist, and facilitator. Her work focuses on creative collaborations within and beyond the label of 'art meets science', multispecies worlds, and the aesthetic dimensions of the workplace and sites of knowledge production like Natural History Museums, Aquariums, or Botanical Gardens."
                    , event dir "Botanizing the Virtual" "https://www.facebook.com/events/1126808091099825/"
                    , Html.h4 [] [ Html.text "Creative Companion:" ]
                    , internal dir "Series 4"
                    , internal dir "Series 5"
                    , internal dir "Series 6"
                    ]
      , photo = "https://lh6.googleusercontent.com/d0BMb7tR4_4y7i92TshMEyTY2R5KTQdGWgya1CBKqjYk8IRcwWq_w7t80zIyv0YCJKCc0KwNW5_UurHosESzxPIZOkqAVzsbWXzpxPfVvCJbJYk_oXKY5XknNlh-viN0P6J0Slp6Mg_kZkKqlg"
      , wide = False
      }
    , { name = "Cinzi\u{00A0}Schincariol"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is interested in improvisation as dance, as performance and composition, as an enquiry and research method. She trained in IDME (Infant Movement Development Education) through the school of Body Mind Centering© and holds a Certificate in Play Therapy. Cinzia has danced with people of many walks of life and abilities, as part of the education team at ‘Restless Dance Theatre - a mixed-ability performance company’ and contributor of ‘Company@ - an autistic theatre company’. Currently based in Berlin, Cinzia performs and regularly hosts Authentic Movement and contemplative dance practice."
                    , weblink "cinziaschincariol.com" "https://www.cinziaschincariol.com/"
                    , event dir "layers of cells/earth/a.../" "https://www.facebook.com/events/531880204465278/"
                    ]
      , photo = "https://lh3.googleusercontent.com/Yv5X5xwlpuchMDoVvolztXrzeT7a2RV5zS78yksnyH07uh4DZX6WKZuWFy-9IIjoDLEchFT8igkW-p4o-O7mRasuTb_htff5nnZgw7T4Z2zM1qlMknRUoFnKmG-KVi1tdzm95trFV2FPwJEclw"
      , wide = True
      }
    , { name = "Maikon\u{00A0}K"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a Brazilian performance artist. He works on the borders between performance, dance and theater. The center of his work is the body and its ability to change perceptions, influenced by the shamanic worldview, in which the performer expands into different realities through specific body-based techniques such as song, non-verbal sound, dance, visual signs and ritualized activities. His training began in 2000 in Performing Arts and includes several areas of knowledge: a graduate degree in Social Sciences (majoring in Anthropology of the Theater) and since 2001 researching ways of modifying consciousness through bodily practices and rites. "
                    , weblink "maikonk.com" "https://maikonk.com/"
                    , event dir "Facing the Impossible" "https://www.facebook.com/events/302012574958346"
                    ]
      , photo = "https://lh6.googleusercontent.com/cRFXa4T1BRErSN7tF-brmEQtcvhUUtqKLaojhjxJoQ2f20Xv5w1JVp3daSgmalq_E03Bm3MZHOLTXIPcLxyO40y-38GiuoMbyUrDCvYJM8wEW1Tb2qHk5cbpnwU-tpkN78mka1RwCVEUSypzfw"
      , wide = False
      }
    , { name = "Siegmar\u{00A0}Zacharias"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a performance artist, researcher, and death doula. She collaborates with humans and uncontrollable materials, such as smoke, slime, drool, the nervous system and death/dying/grief. Together with Steve Heather she is exploring binaural sound for deep listening and deep touch. She is indebted to cultures of listening with the whole body, merging somatic practices, performance practices, herbalism, social work and visceral thinking together. Her works have been shown internationally. She works on intimacy and alienation as two forces of generative transformation."
                    , weblink "siegmarzacharias.com" "http://www.siegmarzacharias.com"
                    , event dir "Wailing is breathing out loud for others" "https://www.facebook.com/events/524518955894480/"
                    ]
      , photo = "https://lh6.googleusercontent.com/p7Fm5qDad-npe-sJRJAtyYzvQx5DYf4Zp9RcO5aqjPMeoNSsGXZdHgSgSU00TCPNP8tl9BjgUWalAIasV33Hc5QPBkbMxacM3NcXZ8MYrAn8VBgS18lEN_0U-PI1R-WpJkRgTLr4rv2sH6IRUA"
      , wide = False
      }
    , { name = "Tsuki"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a Performance Artist, Butoh and Ashtanga Yoga teacher based in Berlin. She leads Full Moon Performance Rituals and is focused on enabling spaces where the art, the community, and the survival can co-exist. Tsuki performs solo and collaborative performance projects influenced by her multidisciplinary training and has performed in Berlins most infamous night clubs and queer parties, including; Tresor, KitKat Club, Suicide Circus."
                    , weblink "breathandbecoming.wixsite.com/tsuki" "https://breathandbecoming.wixsite.com/tsuki/"
                    , event dir "Moon Cancer Lightning" "https://www.facebook.com/events/771616107557809/"
                    ]
      , photo = "https://lh4.googleusercontent.com/w2DGtzW7bmuSQ0erzRGI67uXRMPLiaPbUFiMW57Lh7jvRlPe5OY2G5juI9k597-tsegUMN3jgTnLR4ZsYgKuaAIkrnyfSjdW1hhTVutfmXZ2iI782ZDisqdJdJR48k4KhA4o3KmAc2A2C5HLcQ"
      , wide = True
      }
    , { name = "Roland\u{00A0}Walter"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "was born in Magdeburg in 1963. At birth he was born with a lack of oxygen, which is the cause of his spastic paralysis. Walter lives with assistance in Berlin. In 2010 Roland started working as an artist. As a performer he experiments with his body and works with artists worldwide, showing the audience a change of perspective. Roland also holds lectures and workshops in schools. So far Roland has worked and researched in Berlin at Theaterhaus Mitte and in the Sophiensaele. With his paralyzed body he fascinates the audience. "
                    , weblink "roland-walter.de" "https://roland-walter.de/"
                    , event dir "parallel worlds" "https://www.facebook.com/events/452048022822254/"
                    ]
      , photo = "https://lh4.googleusercontent.com/5P6VEeTon9r50xjK53zEQWANbfFwvZStaJkaQ7HKnJTunweP-4ehzY4jfPD-jjJQlzPmI7JaZaeMthIqRewLljBpSZn364VtBh_G08UDWUdxbcXUE4QkU0oToUDf1H2_gka65lIfLaiRNwL7VQ"
      , wide = False
      }
    , { name = "Isack\u{00A0}Abeneko"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is the founder and creative director of ASEDEVA (Art for Social and Economic Development in Africa), a Tanzanian based non profit arts organization. He performs as a dancer, actor and musician while increasingly working off stage as a choreographer, song writer, and curator of the annual ‘Marafiki Music Festival’ and ‘Haba na Haba International Dance Festival’. Isack is motivated by the power of performance art to express and question cultural diversity and colonial politics."
                    , weblink "asedeva.com" "http://www.asedeva.com/"
                    , festival "Foregrounding the background" "Radialsystem" "https://www.radialsystem.de/en/veranstaltungen/moving-across-thresholds/"
                    ]
      , photo = "https://lh3.googleusercontent.com/vEYuRZmFzFtQfUJO748pxL7mHMJ6uf5uAU5RpUR9WJKeLSRdvfGe4WXPWy2QR-5z7vQNUWUTBCpwoBLcK5SUHEKdwGfWoctKGoMLKbx9hE5FaJCthRmD_eD_Kevs1dIO6q7fWYdc6HJzSga-ig"
      , wide = False
      }
    , { name = "Giovana\u{00A0}de\u{00A0}Souza\u{00A0}Possignolo"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a Brazilian doctoral student in Sociology at Justus Liebig University Giessen and a graduate fellow at the International Graduate Centre for the Study of Culture (GCSC), Germany. In her research, she departs from border, feminist and anti-racist epistemologies to feel-think with peripheral women from the edges of São Paulo city (Brazil) through lived embodied experiences."
                    , festival "Foregrounding the background" "Radialsystem" "https://www.radialsystem.de/en/veranstaltungen/moving-across-thresholds/"
                    ]
      , photo = "https://lh6.googleusercontent.com/zoEGn1A_OOG60YJ41KbZ1ETcAYxWHi3jODBdAcJPVhjErtMrxhCoHYkPKvLXkgOU6vJ5ByoXJpVTBkbw4UvBjDvqGwbL-loWktgFqJKR09hlfvsxwUI7NnSAPqFjam5KSbALB0OswKiz6shbyg"
      , wide = False
      }
    , { name = "Judith\u{00A0}Förster"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a choreographer, dancer, stage and costume designer living in Berlin. Her work is situated between visual art and dance. She combines the performative with the somatic and works experientially with bodies and their political, sensual and visual dimensions. The interaction of body and material is at the center of her research. In collaboration with various choreographers, visual artists and dancers, she continuously gives new forms to the interaction of bodies and materials in her work. "
                    , festival "Foregrounding the background" "Radialsystem" "https://www.radialsystem.de/en/veranstaltungen/moving-across-thresholds/"
                    , weblink "judithfoerster.de" "www.judithfoerster.de"
                    , Html.h4 [] [ Html.text "Creative Companion:" ]
                    , internal dir "Series 1"
                    , internal dir "Series 2"
                    , internal dir "Series 3"
                    ]
      , photo = "https://lh6.googleusercontent.com/VZJG7c7-aTYhn2JuuXzbhSWePR1x3KRJoSllnV2IyRp-TpCVD22bB1Lp_pSiPJ5wvrp0aoFU4SPAtid-QN1_Ei7VEoLKVufrlzqL4TjfnARJBGmWLi3khA1C4BYNnbdFc1OPRdPXBR-ce-RG7g"
      , wide = False
      }
    , { name = "Renae\u{00A0}Shadler"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a Berlin-based choreographer, performer and researcher. As founder of `Renae Shadler & Collaborators`, a project-based collective that develops performances with touring artists and non-professional participants, she has created works for theaters, train stations and city squares. Her choreographic practice `Worlding` (since 2015) explores how we shape and are shaped by our environment, and manifests in her creations, MaT curation/facilitation and the `Worlding` podcast series. "
                    , festival "Foregrounding the background" "Radialsystem" "https://www.radialsystem.de/en/veranstaltungen/moving-across-thresholds/"
                    , weblink "renaeshadler.com" "http://www.renaeshadler.com"
                    , internal dir "Series 1"
                    , internal dir "Series 2"
                    , internal dir "Series 3"
                    , internal dir "Series 4"
                    , internal dir "Series 5"
                    , internal dir "Series 6"
                    ]
      , photo = "https://lh5.googleusercontent.com/jLwyczx8qR-MbNdBfbSYDDk4c-F7fVacKqhefsgVLqdtFIxhH0CEhyFLAMc56XxBQB29zLEd3Ofp0PEhr6H4EpZvItB1vQJ4tH59lo4WcNxKHTWwmydw-nPE1k-DLdfGF11G7wfYkBZih-YlPQ"
      , wide = False
      }
    , { name = "Vera\u{00A0}Shchelkina"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text """born in Moscow, is a dance artist that uses various somatic approaches to research the human body and living matter. Her main dance and performance pieces where she has been a co-author and performer are "CO-TOUCH" (2018, Moscow, Saint-Petersburg, Berlin, Dresden), "The Imposture Lab" (2018, Moscow, Minsk), "My Ex(body) (2018, Moscow, Minsk», SMRTЪ (2020, Moscow, Yekaterinburg, Kazan), «Vibrant Matter» (2021, Saint-Petersburg), and others.

She is recently studying at the Somatische Akademie in Berlin to become a somatic movement educator and regularly teaches workshops and classes. """
                    , weblink "movingtouch.space" "https://www.movingtouch.space/"
                    , event dir "Unplanting the seeds of hatred" "https://www.facebook.com/events/601421931359812"
                    ]
      , photo = "https://lh6.googleusercontent.com/wZH-jLExTmRjdoGwwEpKsJxj2Hg0mXgXM5XU_0QdJ-jTUnDIr-Svru8UJ94-BwO8h_9QiRneDJFp1v4LBE1H9hrxdL4AOrlUycn9TNJ6R58B4In2W7QgxUDkMA-DWJECghiYs1gnNOMtu87LKLZwWqpo5n-7ChoHMFbfIKSMZ2ns9ATSLoQ9gxhycg"
      , wide = True
      }
    , { name = "Ella\u{00A0}von\u{00A0}der\u{00A0}Haide"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text """in her work as garden activist, audio-visual artist, dipl.-ing. spacial planner and manager of a organic community supported agricultural farm (Solawi Isartal) explores techniques to activate alternative sensibilities. She is based in München (Germany)  working  towards ecological and social transformation and is an expert on queer ecologies. Her topics include queer gardening, urban gardening, alternative food systems. """
                    , weblink "eine-andere-welt-ist-pflanzbar.de" "http://www.eine-andere-welt-ist-pflanzbar.de"
                    , event dir "learning to love the microbiome" "https://www.facebook.com/events/3392714897626309"
                    ]
      , photo = "https://lh4.googleusercontent.com/z_3YcxRYXSJ5LaRImksvQnOHbXpXm74eHJSg9IxBbld0UCHIvcgfjVdALvjMQMukhpJf-tlUPi5SNYaspAbN1wWZw3ArK31VhyYNj8sW_Wd2gKarhNkMthTI3w8tjXAcTsgvxx6WLItYIJVQp15pRyepmMpnoPpcoP0invyHPVmDrS3zjT0SKB13OA"
      , wide = True
      }
    , { name = "Robert\u{00A0}Ssempijja"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text """is a Ugandan contemporary artist and dance researcher who works in traditional and non-traditional spaces in an era of post-colonialism and decolonization. In his practice, he presses against the body’s physical limits to create work that is utterly sincere. He addresses social, political and cultural issues, creating meaning through the interplay of the physical body, the psychological world of the mind, human emotions, and the soul. He is committed to constructing a sustainable scene for contemporary dance in Uganda. Ssempijja is also curious to know how the body creates and transfers information and secrets into a movement that builds up the body’s own vocabulary. He assumes that our bodies are comprised of archives of information which are activated when it comes to movement."""
                    , weblink "Robertssempijja.com" "http://Robertssempijja.com"
                    , event dir "Known to Unknown, Alienation" "https://www.facebook.com/events/717565509528106"
                    ]
      , photo = "https://lh4.googleusercontent.com/PBMk2ySDAZA4m8EAR52f_0Te9Vj0WLiG-8HvMnE5ObecQwXoLge7arrZXNRHGwlTBOanyVCY5-RaakKynfxbKAIDq5oG2aPj9Lz6bv9Ff_TWiKVwsrar-mfhdm1FkND5peS-CqDOyzdF-bz8WnEfJb4mVPHo438UI3EwRiRBseH6omCam_uUWSwVaw"
      , wide = False
      }
    , { name = "The\u{00A0}Quilombaque Cultural Community"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text """is an ethnic-cultural political movement ruled by drums. It is situated in the neighborhood of Perus, northwest of the city of São Paulo in Brazil. Created by young cultural activists with the aim of facing the problems and dilemmas that surround the periphery, mainly poverty and violence. Since the beginning, human development through art, culture and knowledge are our instruments and tools for producing effective changes in this reality. Because these, understood and articulated in a systemic way, promote and provide guarantee of access to an essential human right: Emancipation, Expanding and amplifying the imaginary universe and the repertoire of languages; Autonomy, generating work, income and prospects for young people."""
                    , weblink "quilombaque.org.br" "http://quilombaque.org.br/"
                    ]
      , photo = "https://lh6.googleusercontent.com/6dHBFr_R1mYYDDw1s9LA-jZBz0YjN81lvuQiXp5w_ANbbgrUYwlMyI7uSFx_ML7JHbHcd8-sgcrPBPTPuM4RP3-GuNXVhuY2OtlxdaAivHIQnpQDzQjnx7FSmD8Z3AiIVSUp8RlBJnGvnfeO7ouUz6JH7ynvsfSUcXMHCJF6E6_gdHO7m7NtFmXsDQ"
      , wide = True
      }
    , { name = "Jol\u{00A0}Thoms"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text """is a quantum ecologist living and working on a wise and lively Ocean planet. As an artist and educator he works in the fields of art & science for eco-social and epistemic justice. As a vernacular musician Jol is an obsessive mystic. His practice addresses our troubled relationships with nature, technology, and the cosmos by signaling beyond the purely measurable and quantifiable, and by thinking, feeling, and sensing with more-than-human worlds."""
                    , weblink "jolthoms.com" "https://jolthoms.com/"
                    ]
      , photo = "https://lh4.googleusercontent.com/uCGilVHlSpWgj_iAls-JWBJB4sBcOdz40bkYfyzUSs_OQFWabGftO6YWq-x6L00-kTyszU009lxHsYfsVQdkcHS3QBi538Ah07bCxy2CE2VFqCubG8rtIhuO_HFYlJz-qXyCBU2-k3yg5DFrO1isT1GUq1oZjHJuQk_N7aY9BS0htaVkh2A4vAFBfQ"
      , wide = False
      }
    , { name = "Samuel\u{00A0}Hertz"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a Berlin-based sound artist and researcher working at intersections of Earth-based sound, sonic sensualities, and climate change. Having studied composition with experimental music pioneers Pauline Oliveros, Fred Frith, Maggi Payne, and Zeena Parkins at the Center for Contemporary Music at Mills College, Hertz works fluidly between the worlds of composed music for ensembles, electronic music and installation, performance, and film. His work has been seen at venues such as Fylkingen (SE), Ars Electronica Festival (AT), Sophiensaele (DE), the National Science and Media Museum (UK), Opera North (UK), and the Macerata Opera Festival (IT) among many others. Hertz’s writings on the relationships between sound and climate can be found in interdisciplinary journals and platforms, as well as two recent book chapters. Hertz is currently a AHRC/Techne-funded PhD candidate at Royal Holloway, University of London (Centre for GeoHumanities), researching sound-sensing networks of climate change at the scales of atmospheric, geologic, and bioacoustic conservation."
                    , weblink "samhertzsound.com" "http://www.samhertzsound.com"
                    , event dir "Scalar Sensorium" "https://www.facebook.com/events/1241796136377315"
                    ]
      , photo = "https://lh3.googleusercontent.com/MecYX84rnf6hfsKvVM_kvr31KsBbjuxEQiGSoElBlQp6zFFfA7HJ_EGYrBbPwynqmfvW82wUJFg1Por5WBfa0l4tS_hQhih6dLZohpWsxJDbKDPysM9d9JAt5F0-q_8nbzb89Kwlhc1YHISDr7xRsRBRD2hILwsb58ViOiA1YLXC3mZC9Qe4Dm-m-Ns6xg"
      , wide = False
      }
    , { name = "Viviane\u{00A0}Tabach"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text "is a curator and art educator. Her research focuses on educational methods within curatorial and artistic practices. She is both a Masters student in Art in Context at UdK, Berlin, and in Cultures of the Curatorial at HGB, Leipzig. She holds a postgraduate degree in Art: Criticism and Curatorship from Pontifícia Universidade Católica de São Paulo, and a Teaching Practice and BA of Visual Arts degrees from Unesp, São Paulo. She has worked for institutions such as the Pinacoteca Museum in São Paulo, Sesc Pompeia, Sao Paulo Biennial Foundation, Berlin Biennale, among others. She co-founded and co-directs the art space Casa Aberta, in São Paulo, where she holds hybrid projects that weave together arts, dance and music. And she runs the art space Co-Making Matters at Haus der Statistik, Berlin."
                    , festivalEvent dir "Tidal Shifts"
                    ]
      , photo = "https://lh4.googleusercontent.com/pGTJzzuK2e3kaHiu7d6Y9vIBMJODvOWWdcujTDJPWcmX3DY3sqyEXB8q9ZUU63oi3qyif56oSuzxbszNJztqbJPaTvbqgqpB2cT0Ha-41WzNvmzlcYFy9bKHSHhXvV0-mhxT2hK1qa10EE0sPQLtXfSH4b-OH3-QxRniqp9Pv0UryUXvOzI6WTqLN3oCCQ"
      , wide = True
      }
    , { name = "Martha Hincapie\u{00A0}Charry"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text """is a BIPoC Colombian artist: choreographer, performer, decolonial curator and researcher based in Berlin.

Martha studied dance in her home country and completed her dance theatre & solo dance studies at the Folkwang-University Essen under the direction of Pina Bausch and her MA Art in Context at the University of the Arts, UdK Berlin. In 2019 she was Pina Bausch Fellowship recipient.

Her own creations have been invited to festivals and theatres in Europe, Asia and The Americas. She is artistic director of Plataforma/SurReal Berlin Festival. In her curatorial practice she reflects on (de)coloniality processes and forms of survival that artists have experienced in their migration to Europe or as part of local utopias. Since 2021 she is associate curator of the ENCOUNTERS programme at Radialsystem.

She has been opening up a dialogue space between continents through a transdisciplinary reflection of the human body, where the topics of climate chaos, the relationship between art, humans/more-than-humans, and the visible with the invisible world, find a platform. """
                    , weblink "martha-hincapie-charry.com" "http://martha-hincapie-charry.com"
                    , festivalEvent dir "Tidal Shifts"
                    ]
      , photo = "https://lh3.googleusercontent.com/CRNg6FseXSkpjgv0bZFY6FB5V2YRbzzsDIe0DXXgN9W4SCmSHuSxcRtfiflvcB5vI0TZW1ZpgFjVOhzbkxBTLLmuNgO7ALcIhONXu1aY2O9RT7_acXJfM4fjxTnCiZ9ixh9rTnbIUMEQpztz41_qA-KxuvarPtlI27Wm4zg2gLwU2PKcdXjsevu6pxjiZg"
      , wide = True
      }
    , { name = "Anna\u{00A0}Farley"
      , bio =
            \dir ->
                Html.p []
                    [ Html.text " is an autistic artist based in London. Balancing concept and craft, Anna makes process-based art using methods of sculpture, photography, sound, drawing, text, and participation. In pursuit of making an invisible disability visible, Anna consults, creates and delivers bespoke Autism awareness training nationally and internationally. She has worked with Tate, Photofusion, The BFI, Project Art Works, Creativity Works, and is lead inclusivity designer for Place with Purpose. She was nominated and recognized by Dimensions UK as an award winner of the Autism Leaders List 2018 for her contribution to changing communities."
                    , weblink "www.annafarely.co.uk" "http://www.annafarely.co.uk"
                    , Html.a [ href ("/" ++ Layout.sanitise "‘I think this may be a threshold’") ] [ Html.text "Essay ‘I think this may be a threshold’" ]
                    ]
      , photo = "https://movingacrossthresholds.com/asset/artist/Anna%20Farley%20headshot.png"
      , wide = False
      }
    ]
        |> List.sortBy (.name >> String.split "\u{00A0}" >> List.last >> Maybe.withDefault "Y")


view : Artist -> Article.BodyTemplate
view artist =
    (\dir ->
        Html.div [ class "artist richtext" ]
            [ artist.bio dir |> Html.map never ]
    )
        |> Article.Content (Just artist.name)


viewPhoto : Artist -> Article.BodyTemplate
viewPhoto artist =
    cacheImg artist.name
        (if artist.wide then
            4

         else
            2
        )
        "artist"
        artist.photo
        |> always
        |> Article.Illustration
