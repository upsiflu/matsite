module Snippet exposing (view, toString, none, Snippet, cacheImg)

{-| can render Snippets as either Html-String (to store on a DB) or styled elm-css Html -}

import Html.String as Html exposing (Html, text, toHtml)
import Html.String.Attributes exposing (..)
import Html.Styled exposing(fromUnstyled)


{-|-}
type alias Snippet = Html Never

{-|-}
view : Html msg -> Html.Styled.Html msg
view =
            toHtml >> fromUnstyled

{-|-}
toString : Html msg -> String
toString = Html.toString 2

{-|-}
none : Html msg
none = Html.text ""

{-| Caches an optimally downsized image at weserv.nl

  - description: `title` attribute
  - diameter: the desired number of columns (each 21 rem wide) (height will be clamped to 768px)
  - cls: CSS class (space-separated string)
  - location: Url of the original file

-}
cacheImg : String -> Int -> String -> String -> Html msg
cacheImg description diameter cls location =
    let
        columnWidth =
            21 * 16

        ( w, h ) =
            ( String.fromInt (Basics.min 2 diameter * columnWidth), "768" )
    in
    Html.img
        [ title description
        , class cls
        , src <| "https://images.weserv.nl/?url=" ++ location ++ "&w=" ++ w ++ "&h=" ++ h ++ "&fit=inside&we&filename=" ++ description
        ]
        []
