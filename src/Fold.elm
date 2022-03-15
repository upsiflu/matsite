module Fold exposing
    ( Fold
    , Foldr
    , list
    )

{-| Helpers for folding over lists and Zippers

@docs Fold

@docs Foldr


# Helpers

@docs list

-}


{-| The generic type for folding any recursive structure
-}
type alias Fold f a z =
    { f
        | initZ : a -> z
        , add : ( a -> z -> z, a -> z -> z )
    }


{-| When applied in a Zipper.Tree a,

  - `a` is the value type,
  - `aisle` accumulates branches,
  - `z` is a MixedZipper,
  - `zB` is a Zipper of Branches,
  - \`trunk accumulates zippers,
  - `b` is a Branch,
  - `e` is the Tree.

-}
type alias Foldr f a aisle z zB trunk b e =
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



---- Helpers


{-| -}
list : (a -> c -> c) -> List a -> c -> c
list fu l init =
    List.foldl fu init l
