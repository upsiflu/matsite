module Data exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)

--import Zipper.Tree as Tree exposing (Tree)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Tree as Tree exposing (Tree, Direction(..), Walk(..), EdgeOperation(..))


type Data msg
    = Data (Tree (Html msg))

singleton : Tree (Html msg) -> Data msg
singleton = Data


view : Data msg -> Html msg
view (Data tree) =
    tree
        |> Tree.view 
            ( Tree.Default 
                { renderFocus = \x-> Html.div [ css [color (rgb 200 0 0)]] [x]
                , renderPeriphery = identity } 
            ) 
        |> List.singleton
        |> Html.div [ css [backgroundColor (rgb 22 99 11), padding (rem 5)]]


site : Data msg
site =
    let
        walk direction = Tree.walk (Walk direction (Insert placeholder))
        set = always >> Tree.mapBranch

        anarchive = Branch.singleton "Anarchive"
        placeholder = Branch.singleton "?"
        vimeo = Branch.singleton "Vimeo"
        lab = Branch.singleton "Lab"
        festival = Branch.singleton "Festival"
        artist = Branch.singleton "Artist"

        info = Branch.singleton "Info"
        collage = Branch.singleton "Collage"
        description = Branch.singleton "Description"
        video = Branch.singleton "Video"
        credits = Branch.singleton "Video"

        appendSubtree =
            walk Down
                >> set info
                >> walk Right
                >> set collage
                >> walk Right
                >> set description
                >> walk Right
                >> set video
                >> walk Right
                >> set credits
                >> walk Left
                >> walk Left
                >> walk Up

    in
    Tree.fromBranch anarchive
        |> walk Right
        |> set vimeo
        |> walk Right
        |> set lab
        |> appendSubtree
        |> walk Right
        |> set festival
        |> appendSubtree
        |> walk Right
        |> set artist
        |> appendSubtree
        |> walk Up

        |> Tree.map (Html.text)
        |> singleton

anarchiveX : Html msg
anarchiveX =
    Html.iframe 
        [ attribute "width" "100%" 
        , css [ position absolute, Css.height (vh 100), border (px 0) ]
        , src "https://www.are.na/moving-across-thresholds/library-of-worded-companions/embed"
        , title "Moving Across Thresholds - AnArchive"
        ] []

vimeoX : Html msg
vimeoX =
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