module Ui.Aspect exposing (Aspect(..))

{-| -}


{-|

  - control: global toolbar or property sheet
  - info: statusbar, help screen, or tooltip bubbles
  - scene: the item's editable contents and overlays, each with a unique key

-}
type Aspect
    = Scene
    | Control
    | Info
