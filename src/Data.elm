module Data exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)

--import Zipper.Tree as Tree exposing (Tree)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)

site : Branch (Html msg)
site =
    Branch.fromPath (anarchive, [vimeo])
        --|> Branch.fold Branch.defold

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