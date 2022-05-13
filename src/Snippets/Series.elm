module Snippets.Series exposing (..)

import Snippets.Festival exposing (Festival)
import Snippets.Lab exposing (Lab)
import Time


type alias Series =
    { from : Time.Posix
    , until : Time.Posix
    , title : String
    , labs : List Lab
    , festivals : List Festival
    }


series0 =
    { from = Time.millisToPosix 0
    , until = Time.millisToPosix 0
    , title = ""
    , labs = []
    , festivals = []
    }
