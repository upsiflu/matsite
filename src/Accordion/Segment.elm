module Accordion.Segment exposing ( Segment, singleton, empty,withOrientation, ViewMode(..), Orientation(..), view )

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
singleton caption =
    { caption = Just caption 
    , body = Nothing
    , orientation = Vertical
    }

empty : Segment msg
empty =
    { caption = Nothing
    , body = Nothing
    , orientation = Vertical
    }


---- ViewModel ----


{-|-}
type ViewMode
    = Expanded { focused : Bool }
    | Collapsed


---- View and helpers ----

{-|-}
view : ViewMode -> Segment msg -> Html msg
view mode s =
    let
        functions =
            case mode of
            Expanded f -> 
                if f.focused then
                    [id "focus", css [backgroundColor (rgb 0 199 255)]]
                else
                    [class "expanded", css [backgroundColor (rgb 0 99 199)]]
            _ -> [class "collapsed"]

    in
    Html.div functions
        [ (case s.caption of
            Just h -> Html.h3 [] [Html.text h]
            Nothing -> Html.text "?"
            )
            , (case s.body of
            Just b -> b
            Nothing -> Html.text ""
            )
        ]

