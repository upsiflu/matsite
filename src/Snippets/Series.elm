module Snippets.Series exposing (..)

import Snippets.Festival exposing (Festival)
import Snippets.Lab exposing (Lab)


type alias Series =
    { from : Time.Posix
    , until : Time.Posix
    , title : String
    , labs : List Lab
    , festivals : List Festival
    }

series0 =
    { from =
    , until =
    , title = ""
    , labs = []
    , festivals = []
    }