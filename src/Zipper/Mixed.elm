module Zipper.Mixed exposing
    ( MixedZipper
    , singleton, fromZipper, join
    , deviateBy
    , toZipper
    , map, map9, mapFocus
    , left, leftmost
    , right, rightmost
    , insertLeft, prepend
    , insertRight, append
    , insert
    , focus
    , homogenize
    , foldr, foldl
    )

{-| Its `focus` may "deviate" from the other nodes

@docs MixedZipper
@docs singleton, fromZipper, join


## Transform

@docs deviateBy
@docs toZipper


## Map

@docs map, map9, mapFocus


## Navigate

@docs left, leftmost
@docs right, rightmost


## Insert

@docs insertLeft, prepend
@docs insertRight, append
@docs insert


## Deconstruct

@docs focus
@docs homogenize


## Fold

@docs foldr, foldl

-}

import Zipper exposing (Zipper)


{-| -}
type alias MixedZipper f a =
    { aToF : a -> f
    , zipper : Zipper a
    }



---- CREATE ----


{-| -}
singleton : a -> MixedZipper a a
singleton a =
    { aToF = identity
    , zipper = Zipper.singleton a
    }


{-| -}
join : (a -> f) -> a -> List a -> List a -> MixedZipper f a
join fu a l r =
    { aToF = fu
    , zipper = Zipper.create a l r
    }



---- TRANSFORM ----


{-| Composes the `focus` function

    singleton "a"
        |> deviateBy String.toUpper
        |> focus

        --> "A"

-}
deviateBy : (f -> c) -> MixedZipper f a -> MixedZipper c a
deviateBy fu m =
    { aToF = m.aToF >> fu
    , zipper = m.zipper
    }


{-| -}
fromZipper : Zipper a -> MixedZipper a a
fromZipper =
    MixedZipper identity


{-| Requires prior homogenization to explify information loss
-}
toZipper : MixedZipper a a -> Zipper a
toZipper =
    .zipper



---- MAP ----


{-| -}
map : (a -> a) -> MixedZipper f a -> MixedZipper f a
map =
    mapZipper << Zipper.map


mapZipper : (Zipper a -> Zipper a) -> MixedZipper f a -> MixedZipper f a
mapZipper fu m =
    { m | zipper = fu m.zipper }


{-| This is weird
-}
mapFocus : (a -> a) -> MixedZipper f a -> MixedZipper f a
mapFocus =
    mapZipper << Zipper.mapFocus


{-| This is better than mapFocus
-}
map9 : (a -> b) -> (b -> g) -> MixedZipper f a -> MixedZipper g b
map9 fu bToG m =
    { aToF = bToG
    , zipper = Zipper.map fu m.zipper
    }



---- NAVIGATE ----


{-| -}
left : MixedZipper f a -> MixedZipper f a
left =
    mapZipper Zipper.left


{-| -}
right : MixedZipper f a -> MixedZipper f a
right =
    mapZipper Zipper.right


{-| -}
leftmost : MixedZipper f a -> MixedZipper f a
leftmost =
    mapZipper Zipper.leftmost


{-| -}
rightmost : MixedZipper f a -> MixedZipper f a
rightmost =
    mapZipper Zipper.rightmost



---- INSERT ----


{-| -}
insertLeft : a -> MixedZipper f a -> MixedZipper f a
insertLeft =
    mapZipper << Zipper.insertLeft


{-| -}
insertRight : a -> MixedZipper f a -> MixedZipper f a
insertRight =
    mapZipper << Zipper.insertRight


{-| -}
insert : a -> MixedZipper f a -> MixedZipper f a
insert =
    insertRight


{-| -}
prepend : List a -> MixedZipper f a -> MixedZipper f a
prepend =
    mapZipper << Zipper.appendLeft


{-| -}
append : List a -> MixedZipper f a -> MixedZipper f a
append =
    mapZipper << Zipper.appendRight



---- DECONSTRUCT ----


{-| -}
homogenize : MixedZipper f a -> MixedZipper a a
homogenize mz =
    { aToF = identity
    , zipper = mz.zipper
    }


{-| -}
focus : MixedZipper f a -> f
focus m =
    m.zipper |> Zipper.focus |> m.aToF


{-| this is foldr, `cons`ing from both the `left` and `right` leaf and finally
`join`ing with the focus.

    import Zipper exposing (Zipper)
    import Nonempty exposing (Nonempty)

    Zipper [1, 2] 3 [4, 5]
        |> fromZipper
        |> foldr
            { cons = (::)
            , join = Zipper.join
            , left = []
            , right = []
            }

            --> Zipper [1, 2] 3 [4, 5]

-}
foldr :
    { f
        | cons : a -> acc -> acc
        , join : focus -> acc -> acc -> result
        , left : acc
        , right : acc
    }
    -> MixedZipper focus a
    -> result
foldr f z =
    f.join (focus z)
        (List.foldr f.cons f.left z.zipper.left)
        (List.foldr f.cons f.right z.zipper.right)


{-| `acc`umulate the two aisles using `cons`.
The left aisle starts with the first element in the output of `init`,
the right one with the second.
Finally, `join` the resulting tuple.

    import Zipper exposing (Zipper)
    import Nonempty exposing (Nonempty)

    Zipper [1, 2] 3 [4, 5]
        |> fromZipper
        |> foldl
            { cons = Nonempty.appendItem
            , join = \(l, r) ->
                Zipper.join
                    (Nonempty.head l)
                    (Nonempty.tail l)
                    (Nonempty.tail r)
                    |> fromZipper
            , init = \f -> (Nonempty.singleton f, Nonempty.singleton f)
            }
        |> toZipper

            --> Zipper [1, 2] 3 [4, 5]

-}
foldl :
    { f
        | cons : a -> acc -> acc
        , join : ( acc, acc ) -> result
        , init : focus -> ( acc, acc )
    }
    -> MixedZipper focus a
    -> result
foldl f z =
    f.init (focus z)
        |> Tuple.mapBoth
            (\initL -> List.foldl f.cons initL z.zipper.left)
            (\initR -> List.foldl f.cons initR z.zipper.right)
        |> f.join
