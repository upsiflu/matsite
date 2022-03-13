module Fold exposing (Fold)

{-|

@docs Fold

-}


{-| When applied in a Zipper.Tree a,

  - `a` is the value type,
  - `aisle` accumulates branches,
  - `z` is a MixedZipper,
  - `zB` is a Zipper of Branches,
  - `trunk accumulates zippers,
  - `b` is a Branch,
  - `e` is the Tree.

-}
type alias Fold f a aisle z zB trunk b e =
    { f
        | consAisle : b -> aisle -> aisle
        , join : a -> aisle -> aisle -> z
        , joinBranch : b -> aisle -> aisle -> zB
        , consTrunk : z -> trunk -> trunk
        , mergeBranch : a -> trunk -> b
        , mergeTree : zB -> trunk -> e
        , leaf : trunk
        , left : aisle
        , right : aisle
    }
