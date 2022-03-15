module Accordion.Segment exposing ( Segment, singleton, empty,withOrientation,withBody, ViewMode(..), Orientation(..), preferMode, continueDirection,changeDirection, view )

{-|

Segments only contain immutable data.
For the view, they can be overloaded with more information.

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Layout exposing (..)


import Zipper.Tree as Tree exposing (Direction(..))

{-|-}
type alias Segment msg =
    { caption : Maybe String 
    , id : String
    , body : Maybe (Html msg)
    , orientation : Orientation
    }

{-|-}
type Orientation
    = Vertical
    | Horizontal

{-|-}
withOrientation : Orientation -> Segment msg -> Segment msg
withOrientation orientation segment =
    { segment | orientation = orientation }

{-|-}
withBody : Html msg -> Segment msg -> Segment msg
withBody body segment =
    { segment | body = Just body }

singleton : String -> Segment msg
singleton id =
    { caption = Just id 
    , id = String.replace " " "-" id
    , body = Nothing
    , orientation = Vertical
    }

empty : Segment msg
empty =
    { caption = Nothing
    , id = ""
    , body = Nothing
    , orientation = Vertical
    }


---- ViewModel ----


type alias Path
    = List Tree.Direction

{-|-}
changeDirection : Tree.Direction -> ViewMode -> ViewMode
changeDirection dir viewmode =
    case  viewmode of
        Expanded config path -> Expanded config (dir::path) |> Debug.log "changing to"
        Collapsed path -> Collapsed (dir::path) |> Debug.log "changing to"
        Invisible -> Invisible

{-|-}
continueDirection : ViewMode -> ViewMode
continueDirection viewmode =
    case viewmode of
        Expanded config [] -> Expanded config [] |> Debug.log "continuing EMPTY!"
        Expanded config (a::aa) -> Expanded config (a::a::aa) |> Debug.log "continuing"
        Collapsed [] -> Collapsed [] |> Debug.log "continuing EMPTY!"
        Collapsed (a::aa) -> Collapsed (a::a::aa)   
        Invisible -> Invisible


{-|-}
type ViewMode
    = Expanded { focused : Bool } Path
    | Collapsed Path
    | Invisible

{-|-}
preferMode : ViewMode -> ViewMode -> ViewMode
preferMode preference statusQuo =
    case ( preference, statusQuo ) of
        ( _, Invisible ) -> Invisible
        ( Invisible, _ ) -> Invisible
        ( Collapsed path0, Collapsed path1 ) -> Collapsed (path0++path1)
        ( Collapsed path0, Expanded _ path1 ) -> Collapsed (path0++path1)
        ( Expanded _ path0, Collapsed path1 ) -> Collapsed (path0++path1)
        ( Expanded config path0, Expanded _ path1 ) -> Expanded config (path0++path1)


---- View and helpers ----

{-|-}
view : ViewMode -> Segment msg -> Html msg
view mode s =
    let
        default =
            [css 
                [ overflowY scroll
                , Css.width (rem 21)
                ]
            ]

        functions =
            case mode of
                Expanded f path -> 
                    if f.focused then
                        [class "focused"]
                    else
                        [class "expanded"]
                _ -> [class "collapsed"]

        magic =
            case mode of
                Expanded _ _ -> 
                    [css [ maxHeight (calc (vh 100) minus (rem 4))]]
                Collapsed _ -> 
                    [css [ maxHeight (rem 4)], id s.id]
                Invisible -> [css [opacity (num 0.6), visibility Css.hidden]]

        viewDirection : Tree.Direction -> String
        viewDirection dir =
            case dir of
                Left    -> "←"
                Right   -> "→" 
                Up      -> "↑"
                Down    -> "↓"
                Here    -> "⚬"

        directions =
            case mode of
                Expanded config path -> path
                Collapsed path -> path
                Invisible -> []

    in
    Html.div (default ++ functions ++ magic)
        [ (case s.caption of
            Just h -> header s.id h --Html.a [href ("#"++s.id)] [Html.text h]
            Nothing -> Html.text "⤋"
            )
            , Html.text (List.map viewDirection directions |> String.join "")
            --, Html.text "Hoola"
            --, Html.text (List.length directions |> String.fromInt)
            , (case s.body of
            Just b -> b
            Nothing -> Html.text ""
            )
        ]

