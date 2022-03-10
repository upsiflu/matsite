module Data exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)

--import Zipper.Tree as Tree exposing (Tree)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Tree as Tree exposing (Tree)


type Data msg
    = Data (Tree (Html msg))

singleton : Tree (Html msg) -> Data msg
singleton = Data


view : Data msg -> Html msg
view (Data tree) =
    Tree.view identity tree
        |> List.singleton
        |> Html.div [ css [backgroundColor (rgb 22 99 11), padding (rem 5)]]


site : Data msg
site =
    Tree.fromPath (Zipper ["Oldest Content", "Older Content", "Old Content"] "Present Content" ["Future Content"])
        |> Tree.map (Html.text)
        |> singleton

anarchive : Html msg
anarchive =
    Html.iframe 
        [ attribute "width" "100%" 
        , css [ position absolute, Css.height (vh 100), border (px 0) ]
        , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
        , title "Moving Across Thresholds - AnArchive"
        ] []

vimeo : Html msg
vimeo =
    Html.iframe 
        [ attribute "width" "100%" 
        , css [ position absolute, Css.height (vh 100), border (px 0) ]
        , attribute "loading" "lazy"
        , src "https://player.vimeo.com/video/643915247?title=0&amp;byline=0&amp;portrait=0&amp;badge=0&amp;quality=1080p&amp;dnt=1"
        , attribute "frameborder" "0"
        , attribute "allowfullscreen" "allowfullscreen"
        , attribute "data-rocket-lazyload" "fitvidscompatible"
        , attribute "data-lazy-src" "https://player.vimeo.com/video/643915247?title=0&amp;byline=0&amp;portrait=0&amp;badge=0&amp;quality=1080p&amp;dnt=1"
        , attribute "data-ll-status" "loaded"
        , class "entered lazyloaded" 
        ] []