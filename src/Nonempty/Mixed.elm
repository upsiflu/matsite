module Nonempty.Mixed exposing
    ( MixedNonempty
    , singleton, merge
    , map, mapTail, mapHead, mapSecond
    , insert, append, appendMany, prepend
    , cut, delete
    , member, head, tail
    , homogenize
    , justSingleton, isSingleton
    , fold
    )

{-|

@docs MixedNonempty
@docs singleton, merge


## Extend and Shrink

@docs insert, append, appendMany, prepend
@docs cut, delete


## Map

@docs map, mapTail, mapHead, mapSecond


## Deconstruct

@docs member, head, tail
@docs homogenize
@docs justSingleton, isSingleton


## Fold

@docs fold

-}

import Nonempty exposing (Nonempty)


{-| A list with at least one element.
-}
type alias MixedNonempty h a =
    ( h, List a )



---- CREATE ----


{-| -}
merge : h -> List a -> MixedNonempty h a
merge h t =
    ( h, t )


{-| -}
singleton : h -> MixedNonempty h a
singleton h =
    ( h, [] )


{-| -}
member : a -> MixedNonempty h a -> Bool
member x =
    tail >> List.member x


{-| The first parameter is the head mutation, the second for each tail segment -}
map : (a -> b) -> (h -> i) -> MixedNonempty h a -> MixedNonempty i b
map fa fu ( h, t ) =
    ( fu h, List.map fa t )

{-| -}
mapHead : (h -> i) -> MixedNonempty h a -> MixedNonempty i a
mapHead fu =
    map identity fu

{-| -}
mapTail : (a -> b) -> MixedNonempty h a -> MixedNonempty h b
mapTail fa =
    map fa identity


{-| maps the second in a mixed nonempty list -}
mapSecond : Nonempty.TailOperation a -> MixedNonempty h a -> MixedNonempty h a
mapSecond tailOperation ( h, aa ) =
    case aa of
        [] -> ( h, tailOperation.empty ())
        s::ss -> ( h, tailOperation.nonempty s :: ss )



{-| Insert between head and tail -}
insert : a -> MixedNonempty h a -> MixedNonempty h a
insert t (h, ail) =
    (h, t::ail)

{-| Extend the tail towards the end -}
append : a -> MixedNonempty h a -> MixedNonempty h a
append l (h, tai) =
    (h, tai++[l])

{-| Extend the tail towards the end -}
appendMany : List a -> MixedNonempty h a -> MixedNonempty h a
appendMany l (h, tai) =
    (h, tai++l)

{-| Grow a new head and transform the old one into a tail segment -}
prepend : h -> (h->a) -> MixedNonempty h a -> MixedNonempty h a
prepend h fu (t, ail) =
    (h, fu t :: ail)

{-| Given a way to make the first tail segment a new head, Plop out the old head -}
cut : (a -> h) -> MixedNonempty h a -> Maybe (MixedNonempty h a, h)
cut fu (h, ail) =
    case ail of
        [] -> Nothing
        a::il -> Just (merge (fu a) il, h)

{-| Remove the head, provided a default -}
delete : h -> (a -> h) ->  MixedNonempty h a -> MixedNonempty h a
delete default fu =
    cut fu >> Maybe.map Tuple.first >> Maybe.withDefault (singleton default)

{-|-}
homogenize : (h->a) -> MixedNonempty h a -> Nonempty a
homogenize fu (t, ail) =
    (fu t, ail)


{-| -}
fold :
    { f
    | cons : a -> acc -> acc
    , merge : h -> acc -> result
    , leaf : acc
    }
    -> MixedNonempty h a
    -> result
fold f ( h, t ) =
    List.foldr f.cons f.leaf t
        |> f.merge h


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
