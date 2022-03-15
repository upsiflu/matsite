module Zipper.Branch exposing
    ( Branch
    , singleton, merge, fromPath
    , map, mapChildren, mapNode
    , forkLeft, forkRight, fork
    , append, prepend
    , growLeaf, growBranch, growLevel, grow
    , node, children, nextGeneration, allGenerations
    , path
    , isLeaf
    , foldl, foldr, defold
    )

{-|

@docs Branch
@docs singleton, merge, fromPath


## Map

@docs map, mapChildren, mapNode


## Transform

@docs forkLeft, forkRight, fork
@docs append, prepend
@docs growLeaf, growBranch, growLevel, grow


## Deconstruct

@docs node, children, nextGeneration, allGenerations
@docs path
@docs isLeaf


## Fold

@docs foldl, foldr, defold

-}

import Fold exposing (Foldr)
import Nonempty exposing (Nonempty)
import Nonempty.Mixed exposing (MixedNonempty)
import Zipper exposing (Zipper)
import Zipper.Mixed exposing (MixedZipper)


{-| A Zipper.Branch represents a focused path from the root to a leaf in a Tree.
It consists of an initial node and a list of offspring, beginning with the oldest generation.
-}
type Branch a
    = Branch (MixedNonempty a (MixedZipper a (Branch a)))



---- CREATE ----


{-| -}
singleton : a -> Branch a
singleton =
    Nonempty.Mixed.singleton >> Branch


{-| -}
merge : a -> List (MixedZipper a (Branch a)) -> Branch a
merge a =
    Nonempty.Mixed.create a >> Branch


{-| Generate a long Branch without forks
-}
fromPath : Nonempty a -> Branch a
fromPath ( a, aa ) =
    Nonempty.Mixed.create
        a
        (List.map (singleton >> Zipper.Mixed.singleton >> Zipper.Mixed.deviateBy node) aa)
        |> Branch



---- MAP ----


{-|

    singleton 10
        |> map String.fromInt
        |> node

    --> "10"

-}
map : (a -> b) -> Branch a -> Branch b
map fu (Branch br) =
    Nonempty.Mixed.map
        (Zipper.Mixed.map9 (map fu) node)
        fu
        br
        |> Branch


{-| -}
mapNode : (a -> a) -> Branch a -> Branch a
mapNode fu (Branch br) =
    Nonempty.Mixed.mapHead fu
        br
        |> Branch


{-| -}
mapChildren : (a -> a) -> Branch a -> Branch a
mapChildren fu (Branch b) =
    Nonempty.Mixed.mapTail (Zipper.Mixed.map (map fu)) b
        |> Branch


{-| Insert a branch directly left of the focused child. If there are no children, focus the appended branch
-}
forkLeft : Branch a -> Branch a -> Branch a
forkLeft a (Branch b) =
    Nonempty.Mixed.mapSecond
        { nonempty = Zipper.Mixed.insertLeft a
        , empty =
            \_ -> [ Zipper.Mixed.singleton a |> Zipper.Mixed.deviateBy node ]
        }
        b
        |> Branch


{-| Insert a branch directly right of the focused child. If there are no children, focus the appended branch
-}
forkRight : Branch a -> Branch a -> Branch a
forkRight a (Branch b) =
    Nonempty.Mixed.mapSecond
        { nonempty = Zipper.Mixed.insertRight a
        , empty =
            \_ -> [ Zipper.Mixed.singleton a |> Zipper.Mixed.deviateBy node ]
        }
        b
        |> Branch


{-| Insert a branch next to the focused child. The insertion is an implementation detail. If there are no children, focus the appended branch
-}
fork : Branch a -> Branch a -> Branch a
fork a (Branch b) =
    Nonempty.Mixed.mapSecond
        { nonempty = Zipper.Mixed.insert a
        , empty =
            \_ -> [ Zipper.Mixed.singleton a |> Zipper.Mixed.deviateBy node ]
        }
        b
        |> Branch


{-| Prepend a branch left of the children. If there are no children, focus the first appended branch. Fail silently if the list of appendages is empty
-}
prepend : List (Branch a) -> Branch a -> Branch a
prepend aa (Branch b) =
    case aa of
        [] ->
            Branch b

        br :: anch ->
            Nonempty.Mixed.mapSecond
                { nonempty = Zipper.Mixed.prepend aa
                , empty =
                    \_ -> [ Zipper.Mixed.join node br anch [] ]
                }
                b
                |> Branch


{-| Append a branch right of the children. If there are no children, focus the first appended branch. Fail silently if the list of appendages is empty
-}
append : List (Branch a) -> Branch a -> Branch a
append aa (Branch b) =
    case aa of
        [] ->
            Branch b

        br :: anch ->
            Nonempty.Mixed.mapSecond
                { nonempty = Zipper.Mixed.append aa
                , empty =
                    \_ -> [ Zipper.Mixed.join node br [] anch ]
                }
                b
                |> Branch


{-| grows by several levels
-}
grow : List (MixedZipper a (Branch a)) -> Branch a -> Branch a
grow list branch =
    List.foldl
        growLevel
        branch
        list


{-| Add a single leaf to the focused end of this Branch to make it longer
-}
growLeaf : a -> Branch a -> Branch a
growLeaf lf (Branch b) =
    Nonempty.Mixed.add (singleton lf |> Zipper.Mixed.singleton |> Zipper.Mixed.deviateBy node) b
        |> Branch


{-| Add a branch to the focused end of this Branch to make it longer
-}
growBranch : Branch a -> Branch a -> Branch a
growBranch =
    allGenerations >> Nonempty.toList >> grow


{-| -}
allGenerations : Branch a -> Nonempty (MixedZipper a (Branch a))
allGenerations (Branch b) =
    b
        |> Nonempty.Mixed.mapHead
            ------(h->a) -> MixedNonempty h a -> Nonempty a
            (singleton >> Zipper.Mixed.singleton >> Zipper.Mixed.deviateBy node)


{-| Add a branch to the end of this Branch to make it longer
-}
growLevel : MixedZipper a (Branch a) -> Branch a -> Branch a
growLevel lv (Branch b) =
    Nonempty.Mixed.add lv b
        |> Branch



---- DECONSTRUCT ----


{-| `foldr defold ^= identity`
-}
defold : Foldr {} a (List (Branch a)) (MixedZipper a (Branch a)) (Zipper (Branch a)) (List (MixedZipper a (Branch a))) (Branch a) Bool
defold =
    { mergeBranch = merge
    , consTrunk = (::)
    , leaf = []
    , join = \a l r -> Zipper.Mixed.join node (singleton a) l r
    , joinBranch = \a l r -> Zipper.create a l r
    , consAisle = (::)
    , left = []
    , right = []
    , mergeTree = \a b -> False
    }


{-| starts at the focus and accumulates outward

    import Zipper.Mixed
    import Nonempty.Mixed
    import Nonempty
    import Zipper

    [ [singleton ("a")]
    , [fromPath ("b", ["c"])]
    ]
        |> List.indexedMap (\i -> Zipper.Mixed.join node (singleton (String.fromInt i)) [])
        |> merge "root"
        |> foldl
            { init = \n->
                ( Nonempty.singleton (singleton n), Nonempty.singleton (singleton n) )
            , consAisle = Nonempty.Mixed.appendItem
            , join = \(l, r) ->
                Zipper.join
                    ( Nonempty.Mixed.head l )
                    ( Nonempty.Mixed.tail l )
                    ( Nonempty.Mixed.tail r )
                    |> Zipper.Mixed.fromZipper
                    |> Zipper.Mixed.deviateBy node
            , consTrunk = Nonempty.Mixed.appendItem
            , root = Nonempty.Mixed.singleton
            , merge = \(h, t) -> merge h t
            }
        |> path
        --> ("root", ["0", "1"])

-}
foldl :
    { f
        | init : a -> ( aisle, aisle )
        , consAisle : b -> aisle -> aisle
        , join : ( aisle, aisle ) -> z
        , consTrunk : z -> trunk -> trunk
        , root : a -> trunk
        , merge : trunk -> b
    }
    -> Branch a
    -> b
foldl f (Branch b) =
    b
        |> Nonempty.Mixed.foldl
            { -- cons : a -> acc -> acc
              -- init : h -> acc
              cons =
                \zipper trunk ->
                    f.consTrunk
                        (Zipper.Mixed.foldl
                            -- cons : a -> acc -> acc
                            -- join : (acc, acc) -> result
                            -- init : focus -> (acc, acc)
                            { cons = foldl f >> f.consAisle
                            , join = f.join
                            , init = f.init
                            }
                            zipper
                        )
                        trunk
            , init = f.root
            }
        |> f.merge


{-| takes the `leaf`, `left` and `right` accumulators as starting points,
then first folds the zippers horizontally
and finally foldst the generations vertically
-}
foldr :
    { f
        | consAisle : b -> aisle -> aisle
        , join : a -> aisle -> aisle -> z
        , consTrunk : z -> trunk -> trunk
        , mergeBranch : a -> trunk -> b
        , leaf : trunk
        , left : aisle
        , right : aisle
    }
    -> Branch a
    -> b
foldr f (Branch b) =
    b
        |> Nonempty.Mixed.foldr
            { cons =
                \zipper trunk ->
                    f.consTrunk
                        (Zipper.Mixed.foldr
                            { cons = foldr f >> f.consAisle
                            , join = f.join
                            , left = f.left
                            , right = f.right
                            }
                            zipper
                        )
                        trunk
            , merge = f.mergeBranch
            , leaf = f.leaf
            }


{-| -}
node : Branch a -> a
node (Branch b) =
    Nonempty.Mixed.head b


{-| -}
path : Branch a -> Nonempty a
path =
    allGenerations
        >> Nonempty.map Zipper.Mixed.focus


{-| -}
children : Branch a -> List (MixedZipper a (Branch a))
children (Branch b) =
    Nonempty.Mixed.tail b


{-| prunes the focus
-}
nextGeneration : Branch a -> Maybe (Zipper (Branch a))
nextGeneration =
    cut
        >> Maybe.map
            (\( surface, depth ) ->
                Zipper.Mixed.toZipper (Zipper.Mixed.homogenize surface)
                    |> Zipper.mapFocus (always depth)
            )


{-| this is akin to walking one step towards the leaves and making a cut
-}
cut : Branch a -> Maybe ( MixedZipper a (Branch a), Branch a )
cut (Branch b) =
    case b of
        ( h, [] ) ->
            Nothing

        ( h, t :: ail ) ->
            Just
                ( Zipper.Mixed.mapFocus (always (singleton h)) t
                , merge (Zipper.Mixed.focus t) ail
                )


{-| oppposite operation of `cut`
-}
glue : MixedZipper a (Branch a) -> Branch a -> Branch a
glue surface (Branch b) =
    b
        |> Nonempty.Mixed.cons
            (Zipper.Mixed.focus surface)
            (\oldHead -> Zipper.Mixed.deviateBy (always oldHead) surface)
        |> Branch


{-| -}
isLeaf : Branch a -> Bool
isLeaf (Branch b) =
    Nonempty.Mixed.isSingleton b
