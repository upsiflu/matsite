module Zipper.Branch exposing
    ( Branch
    , singleton, create, fromPath
    , map, mapOffspring, mapNode, mapSpine
    , mapLeaves, mapFocusedLeaf
    , cons, insert
    , forkLeft, forkRight
    , prepend, append
    , growRight, growLeft
    , growLeaf, growBranch, growLevel, grow
    , uncons, getLeftmostLeaf, getRightmostLeaf
    , node, children, nextGeneration, allGenerations
    , path
    , isLeaf
    , Fold, fold, defold
    , foldl
    , Foldr, foldr, defoldr
    , DirBranch
    , defoldWithDirections, zipDirections
    , flatFold
    )

{-|

@docs Branch
@docs singleton, create, fromPath


# Map

@docs map, mapOffspring, mapNode, mapSpine
@docs mapLeaves, mapFocusedLeaf


# Grow

@docs cons, insert
@docs forkLeft, forkRight
@docs prepend, append
@docs growRight, growLeft
@docs growLeaf, growBranch, growLevel, grow


# Deconstruct

@docs uncons, getLeftmostLeaf, getRightmostLeaf
@docs node, children, nextGeneration, allGenerations
@docs path
@docs isLeaf


## Fold

@docs Fold, fold, defold

---

@docs foldl
@docs Foldr, foldr, defoldr

---

@docs DirBranch
@docs defoldWithDirections, zipDirections

@docs flatFold

-}

import Fold exposing (Direction(..), Position, Role(..))
import Nonempty exposing (Nonempty)
import Nonempty.Mixed as MixedNonempty exposing (MixedNonempty)
import Result.Extra as Result
import Zipper exposing (Zipper)
import Zipper.Mixed as MixedZipper exposing (MixedZipper)


{-| A Zipper.Branch represents a focused path from the root to a leaf in a Tree.
It consists of an initial node and a list of offspring, beginning with the oldest generation.
-}
type Branch a
    = Branch
        (MixedNonempty
            a
            (MixedZipper a (Branch a))
        )



---- CREATE ----


{-| -}
singleton : a -> Branch a
singleton =
    MixedNonempty.singleton
        >> Branch


{-| -}
create : a -> List (MixedZipper a (Branch a)) -> Branch a
create a =
    MixedNonempty.create a
        >> Branch


{-| Generate a long Branch without forks
-}
fromPath : Nonempty a -> Branch a
fromPath =
    MixedNonempty.mapTail MixedZipper.singleton
        >> Branch



---- MAP ----


{-|

    singleton 10
        |> map String.fromInt
        |> node

    --> "10"

-}
map : (a -> b) -> Branch a -> Branch b
map fu (Branch br) =
    MixedNonempty.map fu
        (MixedZipper.map fu (map fu))
        br
        |> Branch


{-| -}
mapNode : (a -> a) -> Branch a -> Branch a
mapNode fu (Branch br) =
    MixedNonempty.mapHead fu
        br
        |> Branch


{-| -}
mapOffspring : (a -> a) -> Branch a -> Branch a
mapOffspring fu (Branch br) =
    MixedNonempty.mapTail
        (MixedZipper.map fu (map fu))
        br
        |> Branch


{-| maps the nodes without offspring
-}
mapLeaves : (a -> a) -> Branch a -> Branch a
mapLeaves fu (Branch br) =
    MixedNonempty.mapTail
        (MixedZipper.mapPeriphery (mapLeaves fu))
        br
        |> Branch
        |> mapFocusedLeaf fu


{-| maps the focused leaf only
-}
mapFocusedLeaf : (a -> a) -> Branch a -> Branch a
mapFocusedLeaf fu (Branch br) =
    MixedNonempty.mapLast2
        (MixedZipper.mapFocus fu)
        br
        |> Result.extract
            (MixedNonempty.mapHead fu)
        |> Branch


{-| -}
mapSpine : (a -> a) -> Branch a -> Branch a
mapSpine fu (Branch br) =
    MixedNonempty.map fu
        (MixedZipper.mapFocus fu)
        br
        |> Branch


{-| Insert a branch directly left of the focused child. If there are no children, focus the appended branch
-}
forkLeft : Branch a -> Branch a -> Branch a
forkLeft branch (Branch br) =
    MixedNonempty.mapSecond
        { nonempty = MixedZipper.insertLeft branch
        , empty =
            \_ -> [ MixedZipper.singleton (node branch) ]
        }
        br
        |> Branch


{-| Insert a branch directly right of the focused child. If there are no children, focus the appended branch
-}
forkRight : Branch a -> Branch a -> Branch a
forkRight branch (Branch br) =
    MixedNonempty.mapSecond
        { nonempty = MixedZipper.insertRight branch
        , empty =
            \_ -> [ MixedZipper.singleton (node branch) ]
        }
        br
        |> Branch


{-| Prepend a branch left of the children. If there are no children, focus the first appended branch. Fail silently if the list of appendages is empty
-}
prepend : List (Branch a) -> Branch a -> Branch a
prepend aa (Branch br) =
    case aa of
        [] ->
            Branch br

        b :: ranch ->
            MixedNonempty.mapSecond
                { nonempty = MixedZipper.prepend aa
                , empty =
                    \_ -> [ MixedZipper.create (node b) ranch [] ]
                }
                br
                |> Branch


{-| Append a branch right of the children. If there are no children, focus the first appended branch. Fail silently if the list of appendages is empty
-}
append : List (Branch a) -> Branch a -> Branch a
append aa (Branch br) =
    case aa of
        [] ->
            Branch br

        b :: ranch ->
            MixedNonempty.mapSecond
                { nonempty = MixedZipper.append aa
                , empty =
                    \_ -> [ MixedZipper.create (node b) [] ranch ]
                }
                br
                |> Branch


{-| grows by several levels
-}
grow : List (MixedZipper a (Branch a)) -> Branch a -> Branch a
grow list branch =
    List.foldl
        growLevel
        branch
        list


{-| Add a single leaf to the far end of this Branch to make it longer
-}
growLeaf : a -> Branch a -> Branch a
growLeaf lf (Branch br) =
    MixedNonempty.grow (MixedZipper.singleton lf) br
        |> Branch


{-| Add a branch to the focused end of this Branch to make it longer
-}
growBranch : Branch a -> Branch a -> Branch a
growBranch =
    allGenerations >> Nonempty.toList >> grow


{-| appends a branch to the left of the leafmost generation
or under the focus if singleton
-}
growLeft : Branch a -> Branch a -> Branch a
growLeft branch (Branch br) =
    MixedNonempty.mapLast (MixedZipper.growLeft branch) br
        |> Result.unpack
            (\_ -> growBranch branch (Branch br))
            Branch


{-| appends a branch to the right of the leafmost generation
or under the focus if singleton
-}
growRight : Branch a -> Branch a -> Branch a
growRight branch (Branch br) =
    MixedNonempty.mapLast (MixedZipper.growRight branch) br
        |> Result.unpack
            (\_ -> growBranch branch (Branch br))
            Branch


{-| -}
allGenerations : Branch a -> Nonempty (MixedZipper a (Branch a))
allGenerations (Branch br) =
    MixedNonempty.mapHead MixedZipper.singleton br


{-| Add a branch to the end of this Branch to make it longer
-}
growLevel : MixedZipper a (Branch a) -> Branch a -> Branch a
growLevel lv (Branch b) =
    MixedNonempty.grow lv b
        |> Branch



---- DECONSTRUCT ----


{-|

    fromPath (0, [1, 2])
        |> zipDirections
        |> path

        -- (([], 0), [(Down, 1), (Down, 2)])

-}
zipDirections : Branch a -> DirBranch a
zipDirections =
    fold defoldWithDirections


{-| -}
zipPositions : Branch a -> PosBranch a
zipPositions =
    zipDirections
        >> map (Tuple.mapFirst (\dirs -> Fold.directionsToRole dirs |> (\r -> { role = r, isRoot = False, isLeaf = False, path = dirs })))
        >> mapLeaves (Tuple.mapFirst (\pos -> { pos | isLeaf = True }))


{-| -}
leaf : Branch a -> a
leaf =
    allGenerations
        >> Nonempty.last
        >> .focus


{-| -}
getLeftmostLeaf : Branch a -> a
getLeftmostLeaf =
    allGenerations
        >> Nonempty.last
        >> MixedZipper.getLeftmost
        >> Result.unpack identity node


{-| -}
getRightmostLeaf : Branch a -> a
getRightmostLeaf =
    allGenerations
        >> Nonempty.last
        >> MixedZipper.getRightmost
        >> Result.unpack identity node


{-| A fold that tries to adhere very well to the constructor and
cons/append functions and has less functions
-}
type alias Fold f a b =
    { f
        | init : a -> b
        , grow :
            { leftwards : b -> b -> b
            , rightwards : b -> b -> b
            , downwards : a -> b -> b
            }
    }


{-| -}
defold : Fold {} a (Branch a)
defold =
    { init = singleton
    , grow =
        { leftwards = growLeft
        , rightwards = growRight
        , downwards = growLeaf
        }
    }


{-| -}
flatFold : Fold {} a (List a)
flatFold =
    { init = List.singleton
    , grow =
        { leftwards = (++)
        , rightwards = (++)
        , downwards = (::)
        }
    }


{-| -}
type alias DirBranch a =
    Branch ( List Direction, a )


{-| -}
type alias PosBranch a =
    Branch ( Position, a )


type alias Map x =
    x -> x


{-| -}
defoldWithDirections : Fold {} a (Branch ( List Direction, a ))
defoldWithDirections =
    let
        insertDirection :
            Direction
            -> (DirBranch a -> ( List Direction, a ))
            -> Map (DirBranch a -> DirBranch a -> DirBranch a)
        insertDirection dir getReferenceNode build newBranch oldBranch =
            let
                accumulatedPath =
                    Tuple.first (getReferenceNode oldBranch)

                newPath =
                    \subPath ->
                        accumulatedPath
                            ++ dir
                            :: subPath
            in
            map (Tuple.mapFirst newPath) newBranch
                |> (<|) build
                |> (|>) oldBranch
    in
    { init = Tuple.pair [] >> defold.init
    , grow =
        { leftwards =
            insertDirection
                Left
                getLeftmostLeaf
                defold.grow.leftwards
        , rightwards =
            insertDirection
                Right
                getRightmostLeaf
                defold.grow.rightwards
        , downwards =
            \newNode oldBranch ->
                Tuple.first (leaf oldBranch)
                    ++ [ Down ]
                    |> (<|) Tuple.pair
                    |> (|>) newNode
                    |> (<|) defold.grow.downwards
                    |> (|>) oldBranch
        }
    }


{-| simplest fold.

  - start at `node` and `init self`
  - `init` the next generation (MixedZipper) by growing downwards
  - `grow` the next generation by pre-applying `fold f` to the aisle segments,
    and composing the output into the horizontal Branch `grow` functions.

-}
fold :
    Fold f a b
    -> Branch a
    -> b
fold f (Branch br) =
    MixedNonempty.fold
        { init = f.init
        , grow =
            \generation ->
                f.grow.downwards generation.focus
                    >> Fold.list f.grow.leftwards (List.map (fold f) generation.left)
                    >> Fold.list f.grow.rightwards (List.map (fold f) generation.right)
        }
        br



---- Helpers ----


{-| -}
type alias Foldr f a aisle z zB trunk b =
    { f
        | consAisle : b -> aisle -> aisle
        , join : a -> aisle -> aisle -> z
        , joinBranch : b -> aisle -> aisle -> zB
        , consTrunk : z -> trunk -> trunk
        , mergeBranch : a -> trunk -> b
        , leaf : trunk
        , left : aisle
        , right : aisle
    }


{-| `foldr defold ^= identity`
-}
defoldr : Foldr {} a (List (Branch a)) (MixedZipper a (Branch a)) (Zipper (Branch a)) (List (MixedZipper a (Branch a))) (Branch a)
defoldr =
    { mergeBranch = create
    , consTrunk = (::)
    , leaf = []
    , join = MixedZipper.create
    , joinBranch = \a l r -> Zipper.create a l r
    , consAisle = (::)
    , left = []
    , right = []
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
        |> MixedNonempty.foldl
            { -- cons : a -> acc -> acc
              -- init : h -> acc
              cons =
                \zipper trunk ->
                    f.consTrunk
                        (MixedZipper.foldl
                            -- cons : a -> acc -> acc
                            -- init : focus -> (acc, acc)
                            { cons = foldl f >> f.consAisle
                            , init = f.init
                            }
                            zipper
                            |> f.join
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
        |> MixedNonempty.foldr
            { cons =
                \zipper trunk ->
                    f.consTrunk
                        (MixedZipper.foldr
                            { cons = foldr f >> f.consAisle
                            , join = f.join
                            , init =
                                { left = f.left
                                , right = f.right
                                }
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
    MixedNonempty.head b


{-| -}
path : Branch a -> Nonempty a
path =
    allGenerations
        >> Nonempty.map MixedZipper.focus


{-| -}
children : Branch a -> List (MixedZipper a (Branch a))
children (Branch b) =
    MixedNonempty.tail b


{-| prunes the focus
-}
nextGeneration : Branch a -> Maybe (Zipper (Branch a))
nextGeneration (Branch br) =
    MixedNonempty.second br
        |> Maybe.map
            (MixedZipper.mapFocus
                (\single ->
                    Branch br
                        |> uncons
                        |> Tuple.second
                        |> Maybe.withDefault (singleton single)
                )
            )


{-| plops the surface and tries to make the rest another nonempty
-}
uncons : Branch a -> ( MixedZipper a (Branch a), Maybe (Branch a) )
uncons (Branch br) =
    case MixedNonempty.uncons br of
        ( h, Nothing ) ->
            ( MixedZipper.singleton h, Nothing )

        ( h, Just ( g, enerations ) ) ->
            ( MixedZipper.mapFocus (\_ -> h) g, Just (create g.focus enerations) )


{-| opposite operation of `uncons`
-}
cons : MixedZipper a (Branch a) -> Branch a -> Branch a
cons surface (Branch br) =
    br
        |> MixedNonempty.cons
            (\oldHead -> { surface | focus = oldHead })
            surface.focus
        |> Branch


{-| -}
isLeaf : Branch a -> Bool
isLeaf (Branch b) =
    MixedNonempty.isSingleton b


{-| adds between head and tail
-}
insert : MixedZipper a (Branch a) -> Branch a -> Branch a
insert level (Branch b) =
    MixedNonempty.insert level b
        |> Branch
