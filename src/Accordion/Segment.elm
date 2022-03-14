module Accordion.Segment exposing ( Segment, singleton, empty,withOrientation, ViewMode(..), Orientation(..), preferMode, view )

{-|

Segments only contain immutable data.
For the view, they can be overloaded with more information.

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)

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


{-|-}
type ViewMode
    = Expanded { focused : Bool }
    | Collapsed
    | Invisible

{-|-}
preferMode : ViewMode -> ViewMode -> ViewMode
preferMode preference statusQuo =
    case ( preference, statusQuo ) of
        ( _, Invisible ) -> Invisible
        ( Invisible, _ ) -> Invisible
        ( _, Collapsed ) -> Collapsed
        ( preferent, _ ) -> preferent


---- View and helpers ----

{-|-}
view : ViewMode -> Segment msg -> Html msg
view mode s =
    let
        functions =
            case mode of
                Expanded f -> 
                    if f.focused then
                        [class "focused", css [backgroundColor (rgb 0 199 255)]]
                    else
                        [class "expanded", css [backgroundColor (rgb 0 99 199)]]
                _ -> [class "collapsed"]

        magic =
            case mode of
                Invisible -> [css [opacity (num 0.6), visibility Css.hidden]]
                _ -> [id s.id]
    in
    Html.div (functions ++ magic)
        [ (case s.caption of
            Just h -> Html.a [href ("#"++s.id)] [Html.text h]
            Nothing -> Html.text "⤋"
            )
            , (case s.body of
            Just b -> b
            Nothing -> Html.text ""
            )
        ]

