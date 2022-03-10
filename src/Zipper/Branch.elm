module Zipper.Branch exposing
    ( Branch
    , singleton, merge, fromPath
    , map, mapChildren
    , fold, defold
    , node, children, nextGeneration, allGenerations
    , path
    , forkLeft, forkRight, fork
    , append, prepend
    , growLeaf, growBranch, growLevel, grow
    , isLeaf
    )

{-|

@docs Branch
@docs singleton, merge, fromPath


## Map

@docs map, mapChildren


## Transform

@docs forkLeft, forkRight, fork
@docs append, prepend
@docs growLeaf, growBranch, growLevel, grow


## Deconstruct

@docs node, children, nextGeneration, allGenerations
@docs path
@docs isLeaf


## Fold

@docs fold, defold

-}

import Zipper exposing (Zipper)
import Nonempty.Mixed exposing (MixedNonempty)
import Nonempty exposing (Nonempty)
import Zipper.Mixed exposing (MixedZipper)

import Fold exposing (Fold)


{-| A Zipper.Branch represents a focused path from the root to a leaf in a Tree. 
It consists of an initial node and a list of offspring, beginning with the oldest generation. -}
type Branch a =
    Branch ( MixedNonempty a ( MixedZipper a (Branch a) ) )



---- CREATE ----


{-| -}
singleton : a -> Branch a
singleton =
    Nonempty.Mixed.singleton >> Branch


{-| -}
merge : a -> List (MixedZipper a (Branch a)) -> Branch a
merge a =
    Nonempty.Mixed.merge a >> Branch




{-| Generate a long Branch without forks -}
fromPath : Nonempty a -> Branch a
fromPath (a, aa) =
    Nonempty.Mixed.merge
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


{-|-}
mapChildren : (a -> a) -> Branch a -> Branch a
mapChildren fu (Branch b)=
    Nonempty.Mixed.mapTail (Zipper.Mixed.map (map fu)) b 
        |> Branch


{-| Insert a branch directly left of the focused child. If there are no children, focus the appended branch -}
forkLeft : Branch a -> Branch a -> Branch a
forkLeft a (Branch b) =
    Nonempty.Mixed.mapSecond 
        { nonempty = Zipper.Mixed.insertLeft a
        , empty = 
            (\_->[Zipper.Mixed.singleton a |> Zipper.Mixed.deviateBy node])
        }
        b
        |> Branch


{-| Insert a branch directly right of the focused child. If there are no children, focus the appended branch -}
forkRight : Branch a -> Branch a -> Branch a
forkRight a (Branch b) =
    Nonempty.Mixed.mapSecond 
        { nonempty = Zipper.Mixed.insertRight a
        , empty = 
            (\_->[Zipper.Mixed.singleton a |> Zipper.Mixed.deviateBy node])
        }
        b
        |> Branch


{-| Insert a branch next to the focused child. The insertion is an implementation detail. If there are no children, focus the appended branch -}
fork : Branch a -> Branch a -> Branch a
fork a (Branch b) =
    Nonempty.Mixed.mapSecond
        { nonempty = Zipper.Mixed.insert a
        , empty = 
            (\_->[Zipper.Mixed.singleton a |> Zipper.Mixed.deviateBy node])
        }
        b
        |> Branch


{-| Prepend a branch left of the children. If there are no children, focus the first appended branch. Fail silently if the list of appendages is empty -}
prepend : List (Branch a) -> Branch a -> Branch a
prepend aa (Branch b) =
    case aa of
        [] -> Branch b
        br::anch ->
            Nonempty.Mixed.mapSecond
                { nonempty = Zipper.Mixed.prepend aa
                , empty = 
                    (\_->[Zipper.Mixed.join node br anch []])
                }
                b
                |> Branch


{-| Append a branch right of the children. If there are no children, focus the first appended branch. Fail silently if the list of appendages is empty -}
append : List (Branch a) -> Branch a -> Branch a
append aa (Branch b) =
    case aa of
        [] -> Branch b
        br::anch ->
            Nonempty.Mixed.mapSecond
                { nonempty = Zipper.Mixed.append aa
                , empty = 
                    (\_->[Zipper.Mixed.join node br [] anch])
                }
                b
                |> Branch



{-| grows by several levels -}
grow : List ( MixedZipper a (Branch a)) -> Branch a -> Branch a
grow list branch =
    List.foldl
        growLevel
        branch
        list

{-| Add a single leaf to the focused end of this Branch to make it longer -}
growLeaf : a -> Branch a -> Branch a
growLeaf lf (Branch b) =
    Nonempty.Mixed.append (singleton lf |> Zipper.Mixed.singleton |> Zipper.Mixed.deviateBy node ) b
        |> Branch

{-| Add a branch to the focused end of this Branch to make it longer -}
growBranch : Branch a -> Branch a -> Branch a
growBranch =
    allGenerations >> Nonempty.toList >> grow


{-|-}
allGenerations : Branch a -> Nonempty (MixedZipper a (Branch a))
allGenerations (Branch b) =
    b
        |> Nonempty.Mixed.homogenize ------(h->a) -> MixedNonempty h a -> Nonempty a
            ( singleton >> Zipper.Mixed.singleton >> Zipper.Mixed.deviateBy node )

{-| Add a branch to the end of this Branch to make it longer -}
growLevel : MixedZipper a (Branch a) -> Branch a -> Branch a
growLevel lv (Branch b) =
    Nonempty.Mixed.append lv b
        |> Branch


---- DECONSTRUCT ----

 

{-| `fold defold ^= identity` -}            
defold : Fold {} a (List (Branch a)) (MixedZipper a (Branch a)) (Zipper (Branch a)) (List (MixedZipper a (Branch a))) (Branch a) Bool
defold =
    { mergeBranch = merge
    , consTrunk = (::)
    , leaf = []
    , join = (\a l r -> Zipper.Mixed.join node (singleton a) l r)
    , joinBranch = (\a l r -> Zipper.join a l r)
    , consAisle = (::)
    , left = []
    , right = []
    , mergeTree = \a b -> False
    }


{-| -}
fold : 
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
fold f (Branch b) =
    b
      |> Nonempty.Mixed.fold
        { cons = 
            (\zipper trunk ->
                f.consTrunk
                    ( Zipper.Mixed.fold 
                        { cons = fold f >> f.consAisle
                        , join = f.join
                        , left = f.left
                        , right = f.right
                        } 
                        zipper 
                    ) trunk
            )
        , merge = f.mergeBranch
        , leaf = f.leaf
        }


{-| -}
node : Branch a -> a
node ( Branch b )=
    Nonempty.Mixed.head b
{-|-}
path :Branch a -> Nonempty a
path =
    allGenerations
        >> Nonempty.map Zipper.Mixed.focus


{-| -}
children : Branch a -> List (MixedZipper a (Branch a))
children ( Branch b )=
    Nonempty.Mixed.tail b

{-| prunes the focus -}
nextGeneration : Branch a -> Maybe (Zipper (Branch a))
nextGeneration =
    cut >> Maybe.map
        (\(surface, depth) ->
            Zipper.Mixed.toZipper (Zipper.Mixed.homogenize surface)
                |> Zipper.mapFocus (always depth)
        )

{-| this is akin to walking one step towards the leaves and making a cut -}
cut : Branch a -> Maybe (MixedZipper a (Branch a), Branch a)
cut (Branch b) =
    case b of
        ( h, [] ) -> Nothing
        ( h, t::ail ) ->
            Just 
                ( Zipper.Mixed.mapFocus (always (singleton h)) t
                , merge (Zipper.Mixed.focus t) ail
                )

{-| oppposite operation of `cut` -}
glue : MixedZipper a (Branch a) -> Branch a -> Branch a
glue surface (Branch b) =
    b   
        |> Nonempty.Mixed.prepend 
            (Zipper.Mixed.focus surface)
            (\oldHead -> Zipper.Mixed.deviateBy (always oldHead) surface)
        |> Branch




{-|-}
isLeaf : Branch a -> Bool
isLeaf ( Branch b )=
    Nonempty.Mixed.isSingleton b

