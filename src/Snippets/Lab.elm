module Snippets.Lab exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Occurrence exposing (Occurrence)
import Time exposing (Month(..))


type alias Lab =
    { occurance : Occurrence
    , title : String
    , description : Html Never
    , collage : String
    , video : Maybe String
    }


toEnactAMovement : Lab
toEnactAMovement =
    { occurance = Occurrence.moment Time.utc Nov 21 2020 15 30 |> Occurrence.withDurationMinutes 90
    , title = "to enact a movement"
    , description =
        div
            [ Attr.class "richtext"
            ]
            [ h2 []
                [ text "Moving Across Thresholds:"
                , br [] []
                , text "to enact a movement"
                ]
            , p []
                [ text """“I invite you to begin to sink. To sink down into your chair or into the ground if you're lying down.
And dissolve. Dissolve completely into a liquid as if you were not solid anymore, so that no part of you is being held. \u{200B}\u{200B}Feel yourself as the pile of bones wrapped in skin that you are.
And now slowly, with attention, try to move.
How can you move if you are separate pieces?
What needs to happen to initiate and enact a response, a movement?” - Julia Metzger-Traber""" ]
            , p []
                [ text "In this moment, when the Russian invasion is currently devastating Ukraine and there is an ongoing global pandemic, there is a strong political act in coming together to move, to strengthen our bonds, expand our empathy and to enact a movement."
                ]
            , p []
                [ text "In this lab we will explore the anarchic archive of ‘Moving across Thresholds’ scores and stories built over the last 30-events. We will then dissolve this body of work and reconstruct it in a way that generates thresholds of potential to help us think and move now, in these urgent times."
                ]
            , p []
                [ text "> 'movement' noun: position change. > 'movement 'noun: a group of people with a particular set of aims or ideas"
                ]
            , p []
                [ text "> 'movement' noun: position change. > 'movement 'noun: a group of people with a particular set of aims or ideas"
                ]
            ]
    , collage = "asset/05_MAT_2022_IG (1).png"
    , video = Nothing
    }
