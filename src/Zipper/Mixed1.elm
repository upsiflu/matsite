module Zipper.Mixed1 exposing
    ( MixedZipper
    , singleton, create
    , Shuffler
    , left, right
    , leftmost, rightmost
    , map, mapFocus, mapPeriphery
    , deleteFocus
    , growLeft, growRight
    , prepend, append
    , insertLeft, insertListLeft
    , insertRight, insertListRight
    , consLeft, consRight
    , focus, periphery
    , flat
    , isLeftmost, isRightmost, isSingleton
    , length
    , Fold, fold, defold
    , Foldl, foldl, defoldl
    , Foldr, foldr, defoldr
    )

{-| A Zipper that requires a reversible function for navigations

@docs MixedZipper

@docs singleton, create


# Navigate

@docs Shuffler

Note: This zipper wraps over the edges by default. If you want to implement a different behaviour, check [`isLeftmost`](#isLeftmost) and [`isRightmost`](#isRightmost)

@docs left, right
@docs leftmost, rightmost


# Map

@docs map, mapFocus, mapPeriphery


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

---

@docs flat
@docs isLeftmost, isRightmost, isSingleton
@docs length


## Fold

This is actually a CATAMORPHISM and not a traditional fold (I think). But I
still haven't studied category theory...

@docs Fold, fold, defold

---

@docs Foldl, foldl, defoldl

---

@docs Foldr, foldr, defoldr

-}

import Nonempty.Mixed exposing (MixedNonempty)


{-| -}
type alias MixedZipper f a =
    { left : List a
    , focus : f
    , right : List a
    }


{-| -}
singleton : f -> MixedZipper f a
singleton f =
    MixedZipper [] f []


{-| -}
create : f -> List a -> List a -> MixedZipper f a
create f l r =
    MixedZipper l f r


{-|

    focus (singleton 1) --> 1

-}
focus : MixedZipper f a -> f
focus =
    .focus


{-| -}
periphery : MixedZipper f a -> ( List a, List a )
periphery z =
    ( z.left, z.right )


{-| -}
flat : MixedZipper a a -> List a
flat z =
    List.reverse z.left ++ z.focus :: z.right


{-| -}
type alias Shuffler f a =
    ( f -> a, a -> f )


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
rightmost : Shuffler f a -> MixedZipper f a -> MixedZipper f a
rightmost ( fa, af ) z =
    case List.reverse z.right of
        [] ->
            z

        t :: hgir ->
            { z | left = hgir ++ fa z.focus :: z.left, focus = af t, right = [] }


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
left : Shuffler f a -> MixedZipper f a -> MixedZipper f a
left ( fa, af ) z =
    case z.left of
        l :: eft ->
            { z | left = eft, focus = af l, right = fa z.focus :: z.right }

        [] ->
            rightmost ( fa, af ) z


{-| -}
leftmost : Shuffler f a -> MixedZipper f a -> MixedZipper f a
leftmost ( fa, af ) z =
    case List.reverse z.left of
        [] ->
            z

        t :: fel ->
            { z | left = [], focus = af t, right = fel ++ fa z.focus :: z.right }


{-|

    right (singleton "x") |> focus --> "x"

-}
right : Shuffler f a -> MixedZipper f a -> MixedZipper f a
right ( fa, af ) z =
    case z.right of
        r :: ight ->
            { z | left = fa z.focus :: z.left, focus = af r, right = ight }

        [] ->
            leftmost ( fa, af ) z


{-|

    singleton "x"
        |> map String.toUpper

        --> singleton "X"

-}
map : (a -> a) -> MixedZipper a a -> MixedZipper a a
map fu z =
    { left = List.map fu z.left
    , focus = fu z.focus
    , right = List.map fu z.right
    }


{-| -}
mapFocus : (f -> f) -> MixedZipper f a -> MixedZipper f a
mapFocus fu z =
    { z | focus = fu z.focus }


{-| -}
mapPeriphery : (a -> a) -> MixedZipper f a -> MixedZipper f a
mapPeriphery fu z =
    { z | left = List.map fu z.left, right = List.map fu z.right }


{-| -}
deleteFocus : (a -> f) -> MixedZipper f a -> MixedZipper f a
deleteFocus af z =
    case ( z.right, z.left ) of
        ( r :: ight, _ ) ->
            { z | focus = af r, right = ight }

        ( [], l :: eft ) ->
            { z | focus = af l, left = eft }

        _ ->
            z


{-|

    singleton 5
        |> prepend [0, 1, 2, 3, 4]
        |> leftmost
        |> focus

        --> 0

-}
prepend : List a -> MixedZipper f a -> MixedZipper f a
prepend s z =
    { z | left = z.left ++ List.reverse s }


{-|

    singleton 0
        |> append [1, 2, 3, 4, 5]
        |> rightmost
        |> focus

        --> 5

-}
append : List a -> MixedZipper f a -> MixedZipper f a
append s z =
    { z | right = z.right ++ s }


{-| adds a single item onto the left aisle
-}
growLeft : a -> MixedZipper f a -> MixedZipper f a
growLeft =
    List.singleton >> prepend


{-| -}
growRight : a -> MixedZipper f a -> MixedZipper f a
growRight =
    List.singleton >> append


{-| -}
insertLeft : a -> MixedZipper f a -> MixedZipper f a
insertLeft a z =
    { z | left = a :: z.left }


{-| -}
insertRight : a -> MixedZipper f a -> MixedZipper f a
insertRight a z =
    { z | right = a :: z.right }


{-| -}
insertListLeft : List a -> MixedZipper f a -> MixedZipper f a
insertListLeft aa z =
    { z | left = aa ++ z.left }


{-| -}
insertListRight : List a -> MixedZipper f a -> MixedZipper f a
insertListRight aa z =
    { z | right = aa ++ z.right }


{-| push the focus to the right
-}
consLeft : (f -> a) -> f -> MixedZipper f a -> MixedZipper f a
consLeft fa f z =
    { z | focus = f, right = fa z.focus :: z.right }


{-| -}
consRight : (f -> a) -> f -> MixedZipper f a -> MixedZipper f a
consRight fa f z =
    { z | focus = f, left = fa z.focus :: z.left }



----- Deconstruct ----


{-| -}
length : MixedZipper f a -> Int
length z =
    List.length z.left + List.length z.right + 1


{-| -}
isLeftmost : MixedZipper f a -> Bool
isLeftmost z =
    z.left == []


{-| -}
isRightmost : MixedZipper f a -> Bool
isRightmost z =
    z.right == []


{-| -}
isSingleton : MixedZipper f a -> Bool
isSingleton z =
    isRightmost z && isLeftmost z



---- Folding


{-| A fold that tries to adhere very well to the constructor and cons/append functions and has less functions
-}
type alias Fold f h a z =
    { f
        | init : h -> z
        , grow : ( a -> z -> z, a -> z -> z )
    }


{-| -}
fold : Fold f h a z -> MixedZipper h a -> z
fold f zipper =
    f.init zipper.focus
        |> foldThroughList (Tuple.first f.grow) zipper.left
        |> foldThroughList (Tuple.second f.grow) zipper.right


{-|

    fold defold
        (Zipper [0, 1] 2 [3, 4])

        --> (Zipper [0, 1] 2 [3, 4])

-}
defold : Fold {} h a (MixedZipper h a)
defold =
    { init = singleton
    , grow = ( growLeft, growRight )
    }


{-| -}
type alias Foldl f h a acc =
    { f
        | cons : a -> acc -> acc
        , init : h -> ( acc, acc )
    }


{-| `foldl` goes from inside to outside wereas `foldr` reduces from both aisles inwards.
-}
foldl :
    Foldl f h a acc
    -> MixedZipper h a
    -> ( acc, acc )
foldl f z =
    f.init z.focus
        |> Tuple.mapBoth
            (foldThroughList f.cons z.left)
            (foldThroughList f.cons z.right)


{-|

    identity
        ==== defold
        >> Tuple.mapLeft Nonempty.Mixed.tail
        >> (\( l, ( m, r ) ) -> create m l r)

-}
defoldl : Foldl {} h a (MixedNonempty h a)
defoldl =
    { cons = Nonempty.Mixed.grow
    , init = Nonempty.Mixed.singleton >> (\s -> ( s, s ))
    }


{-| -}
type alias Foldr f h a acc result =
    { f
        | cons : a -> acc -> acc
        , join : h -> acc -> acc -> result
        , init : { left : acc, right : acc }
    }


{-| folds the zipper with `cons`, starting from the `left` and `right` initializers
until it reaches the focus which it `join`s to the both `acc`s.
-}
foldr :
    Foldr f h a acc result
    -> MixedZipper h a
    -> result
foldr f z =
    f.join z.focus
        (List.foldr f.cons f.init.left z.left)
        (List.foldr f.cons f.init.right z.right)


{-| -}
defoldr : Foldr {} h a (List a) (MixedZipper h a)
defoldr =
    { cons = (::)
    , join = create
    , init = { left = [], right = [] }
    }



---- Helpers ----


foldThroughList : (a -> z -> z) -> List a -> z -> z
foldThroughList fu list init =
    List.foldl fu init list
