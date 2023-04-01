module Snippets.Gallery exposing (Entry, galleryEntries, view)

import Article
import Directory exposing (Directory)
import Html.String as Html exposing (Html, text)
import Html.String.Attributes exposing (..)
import Layout
import List.Extra as List
import Snippet exposing (cacheImg)


type alias Entry =
    { name : String
    , heading : String
    , credits : String
    , links : Directory -> List (Html Never)
    , photos : List String
    , wide : Bool
    }


galleryEntries : List Entry
galleryEntries =
    let
        internal dir descr =
            Directory.getClosestBy (String.length descr // 4) descr dir
                |> Maybe.map
                    (\uuid -> Html.a [ class "internal", href (Layout.sanitise uuid) ] [ Html.text (" ☛" ++ descr ++ " ") ])
                |> Maybe.withDefault (text descr)
    in
    [ { name = "Gallery - Series 1"
      , heading = "Series 1"
      , links = \dir -> internal dir "Series 1" |> List.singleton
      , credits = "Online only - Photos by Stella Horta"
      , photos =
            [ "asset/gallery/series 1/MAT_19.11 (5).jpg"
            , "asset/gallery/series 1/MAT_19.11 (9).jpg"
            , "asset/gallery/series 1/MAT_19.11 (21).jpg"
            , "asset/gallery/series 1/MAT_19.11 (34).jpg"
            , "asset/gallery/series 1/MAT_19.11 (45).jpg"
            ]
      , wide = False
      }
    , { name = "Gallery - Series 2"
      , heading = "Series 2"
      , links = \dir -> internal dir "Series 2" |> List.singleton
      , credits = "Online only - Photos by Stella Horta"
      , photos =
            [ "asset/gallery/series 2/DSCF2212.jpg"
            , "asset/gallery/series 2/DSCF2285.jpg"
            , "asset/gallery/series 2/DSCF2311.jpg"
            , "asset/gallery/series 2/DSCF2349.jpg"
            , "asset/gallery/series 2/DSCF2510.jpg"
            , "asset/gallery/series 2/DSCF2513.jpg"
            ]
      , wide = False
      }
    , { name = "Gallery - Series 3"
      , heading = "Series 3"
      , links = \dir -> internal dir "Series 3" |> List.singleton
      , credits = "Online only - Photos by Stella Horta"
      , photos =
            [ "asset/gallery/series 3/DSCF5081.jpg"
            , "asset/gallery/series 3/DSCF5121.jpg"
            , "asset/gallery/series 3/DSCF5182.jpg"
            , "asset/gallery/series 3/DSCF5231.jpg"
            , "asset/gallery/series 3/DSCF5241.jpg"
            , "asset/gallery/series 3/DSCF5303.jpg"
            ]
      , wide = False
      }
    , { name = "Gallery - Series 4-6"
      , heading = "Series 4-6"
      , links = \dir -> [ internal dir "Series 4", internal dir "Series 5", internal dir "Series 6" ]
      , credits = "Hybrid online/offline - Photos by Stella Horta"
      , photos =
            [ "asset/gallery/series 4-6/mat_glitch(16).jpg"
            , "asset/gallery/series 4-6/mat_glitch(21).jpg"
            , "asset/gallery/series 4-6/mat_glitch(27).jpg"
            , "asset/gallery/series 4-6/mat_glitch(31).jpg"
            , "asset/gallery/series 4-6/mat_glitch(59).jpg"
            , "asset/gallery/series 4-6/mat_glitch(75).jpg"
            , "asset/gallery/series 4-6/mat_glitch(84).jpg"
            ]
      , wide = False
      }
    , { name = "Gallery - ‘Foregrounding the background' Arnaud Pormarat"
      , heading = "Foregrounding the background - Drawings"
      , links = \dir -> internal dir "Foregrounding the background" |> List.singleton
      , credits = "Event at Radialsystem, Hybrid online/offline - Drawings by Arnaud Pormarat "
      , photos =
            [ "asset/gallery/radialsystem - drawings/radial_1.jpeg"
            , "asset/gallery/radialsystem - drawings/radial_2.jpeg"
            , "asset/gallery/radialsystem - drawings/radial_3.jpeg"
            , "asset/gallery/radialsystem - drawings/radial_4.jpeg"
            ]
      , wide = False
      }
    , { name = "Gallery - ‘Foregrounding the background' Stella Horta"
      , heading = "Foregrounding the background - Photos"
      , links = \dir -> internal dir "Foregrounding the background" |> List.singleton
      , credits = "'Foregrounding the background' at Radialsystem, Hybrid online/offline  - Photos by Stella Horta"
      , photos =
            [ "asset/gallery/radialsystem - pics/2_MAT.jpg"
            , "asset/gallery/radialsystem - pics/3_MAT.jpg"
            , "asset/gallery/radialsystem - pics/4_MAT.jpg"
            , "asset/gallery/radialsystem - pics/5_MAT.jpg"
            , "asset/gallery/radialsystem - pics/6_MAT.jpg"
            , "asset/gallery/radialsystem - pics/7_MAT.jpg"
            , "asset/gallery/radialsystem - pics/8_MAT.jpg"
            , "asset/gallery/radialsystem - pics/9_MAT.jpg"
            , "asset/gallery/radialsystem - pics/10_MAT.jpg"
            , "asset/gallery/radialsystem - pics/11_MAT.jpg"
            , "asset/gallery/radialsystem - pics/12_MAT.jpg"
            ]
      , wide = False
      }
    , { name = "Gallery - Tidal Shifts"
      , heading = "Tidal Shifts"
      , links = \dir -> [ internal dir "Perform[d]ance" ]
      , credits = "'Tidal Shifts' at Perform[d]ance, Hybrid online/offline - Photos by Daria Panteleeva"
      , photos =
            [ "asset/gallery/Tidal shifts/LAB.jpg"
            , "asset/gallery/Tidal shifts/LAB_2 2.jpg"
            , "asset/gallery/Tidal shifts/LAB_13.jpg"
            , "asset/gallery/Tidal shifts/LAB_15.jpg"
            , "asset/gallery/Tidal shifts/LAB_19.jpg"
            , "asset/gallery/Tidal shifts/LAB_31.jpg"
            , "asset/gallery/Tidal shifts/LAB_43.jpg"
            , "asset/gallery/Tidal shifts/LAB_49.jpg"
            , "asset/gallery/Tidal shifts/LAB_58.jpg"
            ]
      , wide = False
      }
    ]


view : Entry -> Article.BodyTemplate
view entry =
    (\dir ->
        Html.div [ class "artist richtext" ]
            (Html.h4 [] [ Html.text entry.credits ]
                :: Html.div [] (List.map (Html.map never) (entry.links dir))
                :: viewPhotos entry
            )
    )
        |> Article.Content (Just entry.heading)


viewPhotos : Entry -> List (Html msg)
viewPhotos entry =
    List.indexedMap
        (\i photo ->
            cacheImg (entry.name ++ " - " ++ String.fromInt i)
                (if entry.wide then
                    4

                 else
                    2
                )
                "entry"
                ("https://movingacrossthresholds.com/" ++ photo)
        )
        entry.photos
