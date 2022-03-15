module Nonempty exposing
    ( Nonempty
    , singleton, create
    , map, mapSecond
    , TailOperation
    , delete, deleteWithDefault
    , grow
    , insert
    , cons
    , append
    , uncons
    , toList
    , member, head, tail
    , justSingleton, isSingleton, length
    , Fold, fold, defold
    , foldl, foldr
    )

{-| A list with at least one element

@docs Nonempty
@docs singleton, create


# Map

@docs map, mapSecond

@docs TailOperation


# Shrink and Grow

@docs delete, deleteWithDefault

@docs grow

@docs insert
@docs cons

@docs append


# Deconstruct

@docs uncons
@docs toList
@docs member, head, tail
@docs justSingleton, isSingleton, length


## Fold

@docs Fold, fold, defold

---

@docs foldl, foldr

-}

import Fold
import List.Extra as List


{-| -}
type alias Nonempty a =
    ( a, List a )


{-| -}
singleton : a -> Nonempty a
singleton a =
    ( a, [] )


{-| -}
create : a -> List a -> Nonempty a
create =
    Tuple.pair



---- Shrink


{-| -}
delete : Nonempty a -> Maybe (Nonempty a)
delete =
    uncons >> Tuple.second


{-| -}
deleteWithDefault : a -> Nonempty a -> Nonempty a
deleteWithDefault default ( h, t ) =
    case t of
        [] ->
            singleton default

        ta :: il ->
            ( ta, il )


{-| Differenctiate functions that behave differently according to whether a list is empty or not
-}
type alias TailOperation a =
    { nonempty : a -> a
    , empty : () -> List a
    }


{-| maps the second in a nonempty list
-}
mapSecond : TailOperation a -> Nonempty a -> Nonempty a
mapSecond tailOperation ( a, aa ) =
    case aa of
        [] ->
            ( a, tailOperation.empty () )

        s :: ss ->
            ( a, tailOperation.nonempty s :: ss )


{-| -}
member : a -> Nonempty a -> Bool
member x =
    toList >> List.member x


{-| Map a function over all members of nonempty.
-}
map : (a -> b) -> Nonempty a -> Nonempty b
map fu ( h, t ) =
    ( fu h, List.map fu t )


{-| Mind: the information that it is nonempty is lost.
-}
toList : Nonempty a -> List a
toList ( h, t ) =
    h :: t



---- Grow and Shrink


{-| -}
grow : a -> Nonempty a -> Nonempty a
grow a ( h, t ) =
    ( h, t ++ [ a ] )


{-| -}
append : List a -> Nonempty a -> Nonempty a
append l ( h, t ) =
    ( h, t ++ l )


{-|

    insert 1 (0, [2])
        --> (0, [1, 2])

-}
insert : a -> Nonempty a -> Nonempty a
insert a ( h, t ) =
    ( h, a :: t )


{-|

    cons 1 (0, [2])
        --> (1, [0, 2])

-}
cons : a -> Nonempty a -> Nonempty a
cons a ( h, t ) =
    ( a, h :: t )


{-| plops the head and tries to make the tail another nonempty
-}
uncons : Nonempty a -> ( a, Maybe (Nonempty a) )
uncons ( h, t ) =
    ( h, List.uncons t )


{-| -}
head : Nonempty a -> a
head ( h, _ ) =
    h


{-| -}
tail : Nonempty a -> List a
tail ( _, t ) =
    t


{-| -}
justSingleton : Nonempty a -> Maybe a
justSingleton ( h, t ) =
    case t of
        [] ->
            Just h

        _ ->
            Nothing


{-| -}
isSingleton : Nonempty a -> Bool
isSingleton =
    tail >> (==) []


{-| -}
length : Nonempty a -> Int
length =
    tail >> List.length >> (+) 1



---- Folding


{-| -}
type alias Fold f a n =
    { f
        | init : a -> n
        , grow : a -> n -> n
    }


{-| -}
fold : Fold f a n -> Nonempty a -> n
fold f ( h, t ) =
    f.init h
        |> Fold.list f.grow t


{-| -}
defold : Fold {} a (Nonempty a)
defold =
    { init = singleton
    , grow = grow
    }


{-| `cons` along the tail, given an `init`ial accumulator
derived from the head.

    foldl
        { cons = appendItem
        , init = singleton
        }
        (0, [1, 2])

        --> (0, [1, 2])

-}
foldl :
    { f
        | cons : a -> acc -> acc
        , init : a -> acc
    }
    -> Nonempty a
    -> acc
foldl f ( h, t ) =
    List.foldl f.cons (f.init h) t


{-| `cons` along the tail, starting from the end (`init`) and
finally `merge`ing with the focus.

    foldr
        { cons = (::)
        , merge = Tuple.pair
        , init = []
        }
        (0, [1, 2])

        --> (0, [1, 2])

-}
foldr :
    { f
        | cons : a -> acc -> acc
        , merge : a -> acc -> result
        , init : acc
    }
    -> Nonempty a
    -> result
foldr f ( h, t ) =
    List.foldr f.cons f.init t
        |> f.merge h
