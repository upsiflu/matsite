module Nonempty exposing
    ( Nonempty
    , singleton
    , insert
    , cons
    , uncons
    , appendItem, appendList
    , map, mapSecond
    , TailOperation
    , fromList
    , toList
    , member, head, tail
    , length
    , justSingleton, isSingleton
    , foldl, foldr
    )

{-|

@docs Nonempty
@docs singleton


## Grow

@docs insert
@docs cons, uncons
@docs appendItem, appendList


## Map

@docs map, mapSecond

@docs TailOperation


## Deconstruct

@docs fromList
@docs toList
@docs member, head, tail
@docs justSingleton, isSingleton, length
@docs foldl, foldr

-}

import List.Extra as List


{-| A list with at least one element.
-}
type alias Nonempty a =
    ( a, List a )


{-| -}
singleton : a -> Nonempty a
singleton a =
    ( a, [] )


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


{-| -}
fold : (a -> b -> b) -> b -> Nonempty a -> b
fold fu previous =
    toList >> List.foldr fu previous


{-| Mind: the information that it is nonempty is lost.
-}
toList : Nonempty a -> List a
toList ( h, t ) =
    h :: t


{-| 

    insert 1 (0, [2])
        --> (0, [1, 2])
-}
insert : a -> Nonempty a -> Nonempty a
insert a ( h, t ) =
    ( h, a :: t )


{-| -}
cons : a -> Nonempty a -> Nonempty a
cons a ( h, t ) =
    ( a, h :: t )

{-| plops the head and tries to make the tail another nonempty -}
uncons : Nonempty a -> (a, Maybe (Nonempty a))
uncons ( h, t ) =
    (h, List.uncons t)


{-| -}
appendItem : a -> Nonempty a -> Nonempty a
appendItem a ( h, t ) =
    ( h, t ++ [ a ] )

{-| -}
appendList : List a -> Nonempty a -> Nonempty a
appendList l ( h, t ) =
    ( h, t ++ l )

{-| Fallable conversion from List.
-}
fromList : List a -> Maybe (Nonempty a)
fromList list =
    case list of
        l :: ist ->
            Just ( l, ist )

        _ ->
            Nothing


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

{-|-}
length : Nonempty a-> Int
length =
    tail >> List.length >> (+) 1
 

---- Folding


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
foldl f (h, t) =
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
foldr f (h, t) =
    List.foldr f.cons f.init t
        |> f.merge h

