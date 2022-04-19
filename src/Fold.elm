module Fold exposing
    ( Fold
    , Foldr
    , Direction(..), viewDirection
    , Position, Role(..), viewPosition
    , list
    , directionsToRole, fataMorganaPosition
    )

{-| Helpers for folding over lists and Zippers

@docs Fold

@docs Foldr


# Directions

@docs Direction, viewDirection


# Position

@docs Position, Role, viewPosition


# Helpers

@docs list

-}


{-| The generic type for folding any recursive structure
-}
type alias Fold f a z =
    { f
        | init : a -> z
        , grow : ( a -> z -> z, a -> z -> z )
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


type alias Position =
    { role : Role, isRoot : Bool, isLeaf : Bool, path : List Direction }


fataMorganaPosition : Position
fataMorganaPosition =
    { role = Child, isRoot = False, isLeaf = False, path = [] }


{-| indicates any `a`'s relation to the `Zipper`'s `focus`:

        B-Ba-Ba
     Ba-P-Ba
    A-A-F-A-A-A
        C

_Other nodes have a role of `Periphery`_

-}
type Role
    = Parent
    | Focus
    | Aisle
    | Child
    | Breadcrumb
    | BreadcrumbAisle
    | Periphery


directionsToRole : List Direction -> Role
directionsToRole dir =
    case dir of
        [] ->
            Focus

        [ Up ] ->
            Parent

        Up :: s ->
            if List.member Down s then
                Periphery

            else if List.all ((==) Up) s then
                Breadcrumb

            else
                BreadcrumbAisle

        [ Down ] ->
            Child

        s ->
            if List.member Down s then
                Periphery

            else
                Aisle


viewPosition : Position -> String
viewPosition pos =
    let
        role =
            case pos.role of
                Parent ->
                    "P"

                Focus ->
                    "F"

                Child ->
                    "C"

                Aisle ->
                    "A"

                Breadcrumb ->
                    "B"

                BreadcrumbAisle ->
                    "Ba"

                Periphery ->
                    "_"

        ( leaf, root ) =
            ( if pos.isLeaf then
                "🍃"

              else
                "\u{1FAB5}"
            , if pos.isRoot then
                "ROOT"

              else
                ""
            )

        pth =
            List.map viewDirection pos.path |> String.join ""
    in
    String.join " " [ role, leaf, root, pth ]


{-| -}
type Direction
    = Left
    | Right
    | Up
    | Down
    | Here


{-| -}
viewDirection : Direction -> String
viewDirection dir =
    case dir of
        Left ->
            "⪪"

        Right ->
            "⪫"

        Up ->
            "⩚"

        Down ->
            "⩛"

        Here ->
            "⚬"



---- Helpers


{-| flip the fold argument order to allow for function composition
-}
list : (a -> c -> c) -> List a -> c -> c
list fu l init =
    List.foldl fu init l
