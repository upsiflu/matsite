module Snippets.Gallery exposing (Entry, galleryEntries, view)

import Article
import Directory exposing (Directory)
import Html.String as Html exposing (Html, text, toHtml)
import Html.String.Attributes exposing (..)
import Html.Styled exposing (fromUnstyled)
import Layout
import List.Extra as List
import Snippet exposing (cacheImg)


type alias Entry =
    { name : String
    , credits : String
    , links : Directory -> Html Never
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
      , links = \dir -> internal dir "Series 1"
      , credits = "Series 1, Online only - Photos by Stella Horta"
      , photos = [ "asset/gallery/series 1/MAT_19.11 (2).jpg" ]
      , wide = False
      }
    ]


view : Entry -> Article.BodyTemplate
view entry =
    (\dir ->
        Html.div [ class "artist richtext" ]
            (Html.map never (entry.links dir)
                :: viewPhotos entry
            )
    )
        |> Article.Content (Just entry.credits)


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
                photo
        )
        entry.photos
