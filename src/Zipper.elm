module Zipper exposing
    ( Zipper
    , singleton, create
    , left, right
    , leftmost, rightmost
    , map, mapFocus, mapPeriphery, mapByPosition
    , deleteFocus
    , growLeft, growRight
    , prepend, append
    , insertLeft, insertListLeft
    , insertRight, insertListRight
    , consLeft, consRight
    , focus, periphery
    , getLeftmost, getRightmost
    , findClosest
    , flat
    , isLeftmost, isRightmost, isSingleton
    , length
    , Fold, fold, defold
    , foldl, foldr
    )

{-|

@docs Zipper
@docs singleton, create


# Navigate

This zipper wraps over the edges by default. If you want to implement a different behaviour, check [`isLeftmost`](#isLeftmost) and [`isRightmost`](#isRightmost)

@docs left, right
@docs leftmost, rightmost


# Map

@docs map, mapFocus, mapPeriphery, mapByPosition


# Shrink and Grow

@docs deleteFocus

---

@docs growLeft, growRight
@docs prepend, append
@docs insertLeft, insertListLeft
@docs insertRight, insertListRight
@docs consLeft, consRight


# Deconstruct

@docs focus, periphery
@docs getLeftmost, getRightmost


# Query

@docs findClosest

---

@docs flat
@docs isLeftmost, isRightmost, isSingleton
@docs length


## Fold

This is actually a CATAMORPHISM and not a traditional fold (I think). But I
still haven't studied category theory...

@docs Fold, fold, defold

---

@docs foldl, foldr

-}

import List.Extra as List


{-| -}
type alias Zipper a =
    { left : List a
    , focus : a
    , right : List a
    }


{-| -}
singleton : a -> Zipper a
singleton a =
    Zipper [] a []


{-| -}
create : a -> List a -> List a -> Zipper a
create a l r =
    Zipper l a r


{-|

    focus (singleton 1) --> 1

-}
focus : Zipper a -> a
focus =
    .focus


{-| goes to the outmost hit it finds. If none found, identity
-}
findClosest : (a -> Bool) -> Zipper a -> Zipper a
findClosest test =
    fold
        { init = singleton
        , grow =
            { leftwards =
                \b ->
                    if test b then
                        growLeft b >> leftmost

                    else
                        growLeft b
            , rightwards =
                \b ->
                    if test b then
                        growRight b >> rightmost

                    else
                        growRight b
            }
        }


{-| -}
periphery : Zipper a -> ( List a, List a )
periphery z =
    ( z.left, z.right )


{-| -}
flat : Zipper a -> List a
flat z =
    List.reverse z.left ++ z.focus :: z.right


{-|

    singleton "x"
        |> insertRight "Y"
        |> insertRight "Z"
        |> insertRight "A"
        |> rightmost
        |> focus

        --> "Y"


    singleton "x"
        |> insertRight "Y"
        |> insertRight "Z"
        |> insertRight "A"
        |> rightmost
        |> length

        --> 4

-}
rightmost : Zipper a -> Zipper a
rightmost z =
    case List.reverse z.right of
        [] ->
            z

        t :: hgir ->
            { z | left = hgir ++ z.focus :: z.left, focus = t, right = [] }


{-|

    singleton "x"
        |> insertLeft "Y"
        |> left
        |> focus

        --> "Y"

    singleton "x"
        |> insertRight "Y"
        |> left
        |> focus

        --> "Y"

    singleton "x"
        |> insertRight "R"
        |> insertLeft "LL"
        |> insertLeft "L"
        |> length

        --> 4

-}
left : Zipper a -> Zipper a
left z =
    case z.left of
        l :: eft ->
            { z | left = eft, focus = l, right = z.focus :: z.right }

        [] ->
            rightmost z


{-| -}
leftmost : Zipper a -> Zipper a
leftmost z =
    case List.reverse z.left of
        [] ->
            z

        t :: fel ->
            { z | left = [], focus = t, right = fel ++ z.focus :: z.right }


{-|

    right (singleton "x") |> focus --> "x"

-}
right : Zipper a -> Zipper a
right z =
    case z.right of
        r :: ight ->
            { z | left = z.focus :: z.left, focus = r, right = ight }

        [] ->
            leftmost z


{-|

    singleton "x"
        |> map String.toUpper

        --> singleton "X"

-}
map : (a -> b) -> Zipper a -> Zipper b
map fu z =
    { left = List.map fu z.left
    , focus = fu z.focus
    , right = List.map fu z.right
    }


{-| -}
mapFocus : (a -> a) -> Zipper a -> Zipper a
mapFocus fu z =
    { z | focus = fu z.focus }


{-| -}
mapPeriphery : (a -> a) -> Zipper a -> Zipper a
mapPeriphery fu z =
    { z | left = List.map fu z.left, right = List.map fu z.right }


{-| -}
mapByPosition : ({ isFocus : Bool } -> a -> b) -> Zipper a -> Zipper b
mapByPosition fu z =
    { left = List.map (fu { isFocus = False }) z.left, focus = fu { isFocus = True } z.focus, right = List.map (fu { isFocus = False }) z.right }


{-| Removes the focused segment

  - go left if there is a segment left

  - otherwise go right if there is a segment right

  - otherwise replace the focus with a default

```
deleteFocus "default" (singleton "X")

--> singleton "default"

singleton "X"
|> insertRight "Y"
|> deleteFocus "default"

--> singleton "Y"

singleton "X"
|> insertLeft "W"
|> deleteFocus "default"

--> singleton "W"

singleton "X"
|> insertLeft "W"
|> insertRight "Y"
|> deleteFocus "default"
|> focus

--> "W"
```

-}
deleteFocus : a -> Zipper a -> Zipper a
deleteFocus default z =
    case ( z.left, z.right ) of
        ( [], [] ) ->
            singleton default

        ( l :: eft, _ ) ->
            { z | left = eft, focus = l }

        ( [], r :: ight ) ->
            { z | right = ight, focus = r }


{-|

    singleton 5
        |> prepend [0, 1, 2, 3, 4]
        |> leftmost
        |> focus

        --> 0

-}
prepend : List a -> Zipper a -> Zipper a
prepend s z =
    { z | left = z.left ++ List.reverse s }


{-|

    singleton 0
        |> append [1, 2, 3, 4, 5]
        |> rightmost
        |> focus

        --> 5

-}
append : List a -> Zipper a -> Zipper a
append s z =
    { z | right = z.right ++ s }


{-| adds a single item onto the left aisle
-}
growLeft : a -> Zipper a -> Zipper a
growLeft =
    List.singleton >> prepend


{-| -}
growRight : a -> Zipper a -> Zipper a
growRight =
    List.singleton >> append


{-| -}
insertLeft : a -> Zipper a -> Zipper a
insertLeft a z =
    { z | left = a :: z.left }


{-| -}
insertRight : a -> Zipper a -> Zipper a
insertRight a z =
    { z | right = a :: z.right }


{-| -}
insertListLeft : List a -> Zipper a -> Zipper a
insertListLeft aa z =
    { z | left = aa ++ z.left }


{-| -}
insertListRight : List a -> Zipper a -> Zipper a
insertListRight aa z =
    { z | right = aa ++ z.right }


{-| push the focus to the right
-}
consLeft : a -> Zipper a -> Zipper a
consLeft a z =
    { z | focus = a, right = z.focus :: z.right }


{-| -}
consRight : a -> Zipper a -> Zipper a
consRight a z =
    { z | focus = a, left = z.focus :: z.left }



----- Deconstruct ----


{-| -}
getLeftmost : Zipper a -> a
getLeftmost z =
    List.last z.left |> Maybe.withDefault z.focus


{-| -}
getRightmost : Zipper a -> a
getRightmost z =
    List.last z.right |> Maybe.withDefault z.focus


{-| -}
length : Zipper a -> Int
length z =
    List.length z.left + List.length z.right + 1


{-| -}
isLeftmost : Zipper a -> Bool
isLeftmost z =
    z.left == []


{-| -}
isRightmost : Zipper a -> Bool
isRightmost z =
    z.right == []


{-| -}
isSingleton : Zipper a -> Bool
isSingleton z =
    isRightmost z && isLeftmost z



---- Folding


{-| A fold that tries to adhere very well to the constructor and cons/append functions and has less functions
-}
type alias Fold f a z =
    { f
        | init : a -> z
        , grow : { leftwards : a -> z -> z, rightwards : a -> z -> z }
    }


{-| -}
fold : Fold f a z -> Zipper a -> z
fold f zipper =
    f.init zipper.focus
        |> applyListFold f.grow.leftwards zipper.left
        |> applyListFold f.grow.rightwards zipper.right


{-|

    fold defold
        (Zipper [0, 1] 2 [3, 4])

        --> (Zipper [0, 1] 2 [3, 4])

-}
defold : Fold {} a (Zipper a)
defold =
    { init = singleton
    , grow = { leftwards = growLeft, rightwards = growRight }
    }


{-| `foldl` goes from inside to outside wereas `foldr` reduces from both aisles inwards.

`init` has already incorporated previous information.
It now swallows the focus.
Then it outputs two accumulators, left and right,
which then `cons` along the aisles.
`join` both aisles to get the `result`.

    import Nonempty

    Zipper [0, 1] 2 [3, 4]
        |> foldl
            { cons = Nonempty.grow
            , join = \(l, r) ->
                Zipper.join
                    (Nonempty.head l)
                    (Nonempty.tail l)
                    (Nonempty.tail r)
            , init = \f -> (Nonempty.singleton f, Nonempty.singleton f)
            }

        --> Zipper [0, 1] 2 [3, 4]

-}
foldl :
    { f
        | cons : a -> acc -> acc
        , join : ( acc, acc ) -> result
        , init : a -> ( acc, acc )
    }
    -> Zipper a
    -> result
foldl f z =
    f.init z.focus
        |> Tuple.mapBoth
            (\initL -> List.foldl f.cons initL z.left)
            (\initR -> List.foldl f.cons initR z.right)
        |> f.join


{-| folds the zipper with `cons`, starting from the `left` and `right` initializers
until it reaches the focus which it `join`s to the both `acc`s.
-}
foldr :
    { f
        | cons : a -> acc -> acc
        , join : a -> acc -> acc -> result
        , left : acc
        , right : acc
    }
    -> Zipper a
    -> result
foldr f z =
    f.join z.focus
        (List.foldr f.cons f.left z.left)
        (List.foldr f.cons f.right z.right)



---- Helpers ----


applyListFold : (a -> z -> z) -> List a -> z -> z
applyListFold fu list init =
    List.foldl fu init list
