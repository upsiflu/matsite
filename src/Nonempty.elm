module Nonempty exposing
    ( Nonempty
    , TailOperation
    , singleton
    , map, mapSecond
    , fromList
    , toList
    , member, head
    , justSingleton, isSingleton
    , fold
    )

{-|

@docs Nonempty
@docs singleton


## Map

@docs map, mapSecond

@docs TailOperation

## Deconstruct

@docs fromList
@docs toList
@docs member, head
@docs justSingleton, isSingleton
@docs fold

-}


{-| A list with at least one element.
-}
type alias Nonempty a =
    ( a, List a )


{-| -}
singleton : a -> Nonempty a
singleton a =
    ( a, [] )


{-| Differenctiate functions that behave differently according to whether a list is empty or not -}
type alias TailOperation a =
    { nonempty : a -> a
    , empty : () -> List a
    }

{-| maps the second in a nonempty list -}
mapSecond : TailOperation a -> Nonempty a -> Nonempty a
mapSecond tailOperation ( a, aa ) =
    case aa of
        [] -> ( a, tailOperation.empty () )
        s::ss -> ( a, tailOperation.nonempty s :: ss )

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
