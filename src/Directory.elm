module Directory exposing (Directory, fromList, get, getClosestBy, insert, viewLink)

{-|

@docs Directory, fromList, get, getClosestBy, insert, viewLink

-}

import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Layout
import Levenshtein
import List.Extra as List
import Maybe.Extra as Maybe


{-| Map from a search term to the id of the page.
Several search terms can yield the same page but a search term cannot yield two different pages.
You can add search terms that point to other search terms; they will be consolidated to
point to the corresponding page.
-}
type alias Directory =
    Dict String String


{-| -}
fromList : List ( String, String ) -> Directory
fromList =
    Dict.fromList >> consolidate


{-| Consolidation allows for a key to reference another key,
so `insert 'a' 'b' >> insert 'b' 'c'` is equivalent to `insert 'a' 'c'`.
-}
insert : String -> String -> Directory -> Directory
insert k v =
    Dict.insert k v >> consolidate


consolidate : Directory -> Directory
consolidate dir =
    let
        traceV ( k, v ) =
            Dict.get v dir
                |> Maybe.map (\ultimateV -> Dict.insert k ultimateV {- >> traceV ( k, ultimateV ) -})
                |> Maybe.withDefault identity
    in
    Dict.toList dir
        |> List.foldl traceV dir


{-| -}
get : String -> Directory -> Maybe String
get =
    Dict.get


{-| finds the closest match within a certain Levenshtein distance
-}
getClosestBy : Int -> String -> Directory -> Maybe String
getClosestBy proximity str dir =
    get str dir
        |> Maybe.orElseLazy
            (\_ ->
                Dict.toList dir
                    |> List.map (\( key, value ) -> ( Levenshtein.distance str key, value ))
                    |> List.minimumBy Tuple.first
                    |> Maybe.filter (Tuple.first >> (>) proximity)
                    |> Maybe.map Tuple.second
            )



---- VIEW ----


viewLink : String -> Directory -> Html msg
viewLink str =
    getClosestBy (String.length str // 4) str
        >> Maybe.map
            (\uuid -> a [ href (Layout.sanitise uuid) ] [ text str ])
        >> Maybe.withDefault (text str)
