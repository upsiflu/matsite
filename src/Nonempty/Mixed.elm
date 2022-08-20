module Nonempty.Mixed exposing
    ( MixedNonempty
    , singleton, create
    , map, mapTail, mapHead, mapSecond, mapLast, Error, mapLast2
    , delete, deleteWithDefault
    , grow
    , insert, append, cons
    , member, head, tail
    , uncons, cut, last, second
    , justSingleton, isSingleton
    , Fold, fold, defold
    , foldl, foldr
    )

{-|

@docs MixedNonempty
@docs singleton, create


# Map

@docs map, mapTail, mapHead, mapSecond, mapLast, Error, mapLast2


# Shrink

@docs delete, deleteWithDefault


# Grow

@docs grow

@docs insert, append, cons


# Deconstruct

@docs member, head, tail
@docs uncons, cut, last, second
@docs justSingleton, isSingleton


## Fold

@docs Fold, fold, defold

---

@docs foldl, foldr

-}

import Fold
import List.Extra as List
import Nonempty exposing (Nonempty)


{-| A list with at least one element.

Note that `MixedNonempty a a == Nonempty z`, no need to transform them.

-}
type alias MixedNonempty h a =
    ( h, List a )



---- CREATE ----


{-| -}
create : h -> List a -> MixedNonempty h a
create h t =
    ( h, t )


{-| -}
singleton : h -> MixedNonempty h a
singleton h =
    ( h, [] )


{-| -}
member : a -> MixedNonempty h a -> Bool
member x =
    tail >> List.member x


{-| The first parameter is the head mutation, the second for each tail segment
-}
map : (h -> i) -> (a -> b) -> MixedNonempty h a -> MixedNonempty i b
map fu fa ( h, t ) =
    ( fu h, List.map fa t )


{-| -}
mapHead : (h -> i) -> MixedNonempty h a -> MixedNonempty i a
mapHead fu =
    map fu identity


{-| -}
mapTail : (a -> b) -> MixedNonempty h a -> MixedNonempty h b
mapTail fu =
    map identity fu


{-| maps the second in a mixed nonempty list
-}
mapSecond : Nonempty.TailOperation a -> MixedNonempty h a -> MixedNonempty h a
mapSecond tailOperation ( h, aa ) =
    case aa of
        [] ->
            ( h, tailOperation.empty () )

        s :: ss ->
            ( h, tailOperation.nonempty s :: ss )


{-| -}
type Error a h
    = OutOfBounds (MixedNonempty h a)


{-| -}
mapLast : (a -> a) -> MixedNonempty h a -> Result (Error a h) (MixedNonempty h a)
mapLast fu ( h, aa ) =
    case List.reverse aa of
        [] ->
            Err (OutOfBounds ( h, aa ))

        t :: sil ->
            Ok ( h, List.reverse (fu t :: sil) )


{-| -}
mapLast2 : (a -> a) -> MixedNonempty h a -> Result (MixedNonempty h a) (MixedNonempty h a)
mapLast2 fu ( h, aa ) =
    case List.reverse aa of
        [] ->
            Err ( h, [] )

        t :: sil ->
            Ok ( h, List.reverse (fu t :: sil) )


{-| -}
delete : MixedNonempty h a -> Maybe (MixedNonempty a a)
delete =
    uncons >> Tuple.second


{-| -}
deleteWithDefault : h -> (a -> h) -> MixedNonempty h a -> MixedNonempty h a
deleteWithDefault default fu =
    cut fu >> Maybe.map Tuple.first >> Maybe.withDefault (singleton default)


{-| plops the head and tries to make the tail another nonempty
-}
uncons : MixedNonempty h a -> ( h, Maybe (Nonempty a) )
uncons ( h, t ) =
    ( h, List.uncons t )


{-| Given a way to make the first tail segment a new head, Plop out the old head.

It diverges from `uncons` in that it keeps the type

-}
cut : (a -> h) -> MixedNonempty h a -> Maybe ( MixedNonempty h a, h )
cut fu ( h, ail ) =
    case ail of
        [] ->
            Nothing

        a :: il ->
            Just ( create (fu a) il, h )


{-| Extend the tail towards the end
-}
grow : a -> MixedNonempty h a -> MixedNonempty h a
grow l ( h, tai ) =
    ( h, tai ++ [ l ] )


{-| Insert between head and tail
-}
insert : a -> MixedNonempty h a -> MixedNonempty h a
insert t ( h, ail ) =
    ( h, t :: ail )


{-| Extend the tail towards the end
-}
append : List a -> MixedNonempty h a -> MixedNonempty h a
append l ( h, tai ) =
    ( h, tai ++ l )


{-| Grow a new head and transform the old one into a tail segment
-}
cons : (h -> a) -> h -> MixedNonempty h a -> MixedNonempty h a
cons fu h ( t, ail ) =
    ( h, fu t :: ail )



---- Fold


{-| -}
type alias Fold f h a n =
    { f
        | init : h -> n
        , grow : a -> n -> n
    }


{-| -}
fold : Fold f h a n -> MixedNonempty h a -> n
fold f ( h, t ) =
    f.init h
        |> Fold.list f.grow t


{-| -}
defold : Fold {} h a (MixedNonempty h a)
defold =
    { init = singleton
    , grow = grow
    }


{-| -}
foldr :
    { f
        | cons : a -> acc -> acc
        , merge : h -> acc -> result
        , leaf : acc
    }
    -> MixedNonempty h a
    -> result
foldr f ( h, t ) =
    List.foldr f.cons f.leaf t
        |> f.merge h


{-|

    foldl
        { cons = grow
        , init = singleton
        }
        ("a", [1, 2])

        --> ("a", [1, 2])

-}
foldl :
    { f
        | cons : a -> acc -> acc
        , init : h -> acc
    }
    -> MixedNonempty h a
    -> acc
foldl f ( h, t ) =
    List.foldl f.cons (f.init h) t


{-| -}
head : MixedNonempty h a -> h
head ( h, _ ) =
    h


{-| -}
tail : MixedNonempty h a -> List a
tail ( _, t ) =
    t


{-| -}
justSingleton : MixedNonempty h a -> Maybe h
justSingleton ( h, t ) =
    case t of
        [] ->
            Just h

        _ ->
            Nothing


{-| -}
isSingleton : MixedNonempty h a -> Bool
isSingleton =
    tail >> (==) []


{-| -}
second : MixedNonempty h b -> Maybe b
second ( h, t ) =
    case t of
        [] ->
            Nothing

        a :: _ ->
            Just a


{-| -}
last : MixedNonempty h b -> Result h b
last ( h, t ) =
    List.last t
        |> Result.fromMaybe h
