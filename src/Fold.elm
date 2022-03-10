module Fold exposing (Fold)

{-|

When applied in a Zipper.Tree a,

- `b` is a Branch,
- `aisle` accumulates branches,
- `z` is a Zipper,
- `trunk accumulates zippers,
- `e` is the Tree.

Here is an illustration of a Fold over a Zipper.Tree:
![](Assets/22-03-09.pdf)

-}
type  alias Fold f a aisle z trunk b e =
    { f
    | consAisle : b -> aisle -> aisle
    , join : a -> aisle -> aisle -> z
    , consTrunk : z -> trunk -> trunk
    , mergeBranch : a -> trunk -> b
    , mergeTree : b -> trunk -> e
    , leaf : trunk
    , left : aisle
    , right : aisle
    }