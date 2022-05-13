module Fold exposing
    ( Fold
    , Foldr
    , Direction(..)
    , decodeDirection, encodeDirection
    , directionFromString, directionToString
    , directionsToRole
    , Position
    , Role(..)
    , positionToString
    , fataMorganaPosition
    , list
    )

{-| Folding over (nonempty) Lists and Zippers

@docs Fold

@docs Foldr


# Directions

@docs Direction
@docs decodeDirection, encodeDirection

---

@docs directionFromString, directionToString
@docs directionsToRole


# Position

@docs Position
@docs Role

---

@docs positionToString

---

@docs fataMorganaPosition


# Helpers

@docs list

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


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


{-| -}
type alias Position =
    { role : Role, isRoot : Bool, isLeaf : Bool, path : List Direction }


{-| -}
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


{-| -}
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


{-| -}
positionToString : Position -> String
positionToString pos =
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
            List.map directionToString pos.path |> String.join ""
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
directionToString : Direction -> String
directionToString dir =
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


{-| -}
directionFromString : String -> Maybe Direction
directionFromString str =
    case str of
        "⪪" ->
            Just Left

        "⪫" ->
            Just Right

        "⩚" ->
            Just Up

        "⩛" ->
            Just Down

        "⚬" ->
            Just Here

        _ ->
            Nothing


{-| -}
decodeDirection : Decoder Direction
decodeDirection =
    Decode.string
        |> Decode.andThen
            (directionFromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unknown Constructor for Fold.Direction")
            )


{-| -}
encodeDirection : Direction -> Value
encodeDirection =
    directionToString >> Encode.string



---- Helpers


{-| flip the fold argument order to allow for function composition
-}
list : (a -> c -> c) -> List a -> c -> c
list fu l init =
    List.foldl fu init l
