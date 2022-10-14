module Zipper.Tree exposing
    ( Tree
    , singleton, create
    , animate, fromPath, fromBranch
    , join, split, Split
    , left, leftmost
    , right, rightmost
    , up, down
    , root, leaf
    , go, Walk(..), Edge(..), EdgeOperation(..)
    , map, mapFocus, mapBranch, mapAisles, mapAisleNodes, mapTrace, mapSpine
    , mapByPosition, mapFocusedLeaf, mapLeaves, mapRoot, mapRoots
    , deleteFocus, deleteIfPossible
    , growRoot, growLeaf, growBranch
    , growLeft, growRight
    , growRootLeft, growRootRight
    , insert, insertLeft, insertRight
    , prepend, append
    , consLeft, consRight
    , any
    , focus, focusedBranch, getLeftmostRoot, getRightmostRoot, getAisleNodes
    , isRoot
    , path, breadcrumbs
    , circumference
    , petrify
    , flatten, flatMap
    , toDict
    , Fold, defold, fold
    , foldr, defoldr
    , DirTree, defoldWithDirections, zipDirections
    , ViewMode(..), view
    )

{-| A nonempty List of branches 🌿 that can be navigated horizontally and vertically.

  - When walking left or right, you will wrap silently because we assume that the branches are set in a circle.
  - Check out [`go`](#go) for alternative navigation methods!


## To Do

  - [ ] Change the `go`/`EdgeOperation` interface to use `Result`
  - [x] Move the `Role` interface from Article.ViewMode to here, change it into `Position`

---

@docs Tree
@docs singleton, create

---

@docs animate, fromPath, fromBranch
@docs join, split, Split


# Navigate

@docs left, leftmost
@docs right, rightmost
@docs up, down
@docs root, leaf


## Constructive Navigation

@docs go, Walk, Edge, EdgeOperation


# Map

@docs map, mapFocus, mapBranch, mapAisles, mapAisleNodes, mapTrace, mapSpine
@docs mapByPosition, mapFocusedLeaf, mapLeaves, mapRoot, mapRoots


## Shrink and Grow

@docs deleteFocus, deleteIfPossible

@docs growRoot, growLeaf, growBranch
@docs growLeft, growRight
@docs growRootLeft, growRootRight

---

@docs insert, insertLeft, insertRight
@docs prepend, append
@docs consLeft, consRight


# Query

@docs any


# Deconstruct

@docs focus, focusedBranch, getLeftmostRoot, getRightmostRoot, getAisleNodes

---

@docs isRoot
@docs path, breadcrumbs
@docs circumference
@docs petrify
@docs flatten, flatMap
@docs toDict


## Fold

@docs Fold, defold, fold

---

@docs foldr, defoldr

---

@docs DirTree, defoldWithDirections, zipDirections


## View

@docs ViewMode, view

-}

import Css exposing (..)
import Dict exposing (Dict)
import Fold exposing (Direction(..), Foldr, Position, Role(..))
import Html.Styled.Attributes exposing (..)
import List.Extra as List
import Nonempty
import Nonempty.Mixed as MixedNonempty exposing (MixedNonempty)
import Result.Extra as Result
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Zipper.Mixed as MixedZipper exposing (MixedZipper)


{-| The Zipper Tree has a head (the several branching lower levels) as well as a tail (the list of upper levels, each having a focus and perhaps alternative branches to go down into).
Its focus is the node of the branch that is focused in the zipper that is the Tree's head.
-}
type alias Tree a =
    MixedNonempty
        (Zipper (Branch a))
        (MixedZipper a (Branch a))



---- CREATE ----


{-| -}
singleton : a -> Tree a
singleton =
    Branch.singleton >> Zipper.singleton >> MixedNonempty.singleton


{-| -}
fromBranch : Branch a -> Tree a
fromBranch =
    Zipper.singleton >> animate


{-| creates an oblong Tree
-}
fromPath : Zipper a -> Tree a
fromPath z =
    let
        createLevel : a -> MixedZipper a (Branch a)
        createLevel =
            Branch.singleton
                >> MixedZipper.singleton
                >> MixedZipper.mapFocus Branch.node
    in
    join
        { past = List.map createLevel z.left
        , present = createLevel z.focus
        , future = List.map createLevel z.right
        }


{-| reads all focused values, from past to future
-}
path : Tree a -> Zipper a
path =
    split
        >> (\s ->
                Zipper
                    (List.map .focus s.past)
                    (.focus s.present)
                    (List.map .focus s.future)
           )


{-| Items in the past
-}
breadcrumbs : Tree a -> List a
breadcrumbs ( _, past ) =
    List.map .focus past


{-| creates a Tree without past

    import Zipper.Branch as Branch
    import Zipper exposing (Zipper)

    Zipper ["left"] "center" ["right"]
        |> Zipper.map (Branch.singleton >> Branch.growLeaf "leaf")
        |> animate
        |> path

        --> Zipper [] "center" ["leaf"]

-}
animate : Zipper (Branch a) -> Tree a
animate =
    MixedNonempty.singleton


{-| internal representation
-}
create : Zipper (Branch a) -> List (MixedZipper a (Branch a)) -> Tree a
create =
    MixedNonempty.create


{-| goes into the past, wraps by default

    import Zipper exposing (Zipper)


    fromPath (Zipper [0] 1 [2])
        |> up
        |> focus

        --> 0


    fromPath (Zipper [0] 1 [2])
        |> up
        |> up
        |> focus

        --> 2

-}
up : Tree a -> Tree a
up =
    split
        >> (\s ->
                join <|
                    case ( s.past, s.future ) of
                        -- no past:
                        ( [], future ) ->
                            case List.reverse future of
                                [] ->
                                    s

                                e :: rutuf ->
                                    { past = rutuf ++ [ s.present ]
                                    , present = e
                                    , future = []
                                    }

                        -- there is past:
                        ( p :: ast, future ) ->
                            { past = ast
                            , present = p
                            , future = s.present :: future
                            }
           )


{-| goes into the future, wraps by default

    singleton 0
        |> down

        --> singleton 0


    import Zipper exposing (Zipper)

    fromPath (Zipper [0] 1 [2])
        |> down
        |> focus

        --> 2


    fromPath (Zipper [0] 1 [2])
        |> down
        |> down
        |> focus

        --> 0

-}
down : Tree a -> Tree a
down =
    split
        >> (\s ->
                join <|
                    case ( s.past, s.future ) of
                        -- no future:
                        ( past, [] ) ->
                            case List.reverse past of
                                [] ->
                                    s

                                t :: sap ->
                                    { past = []
                                    , present = t
                                    , future = sap ++ [ s.present ]
                                    }

                        -- there is future:
                        ( past, f :: uture ) ->
                            { past = s.present :: past
                            , present = f
                            , future = uture
                            }
           )


{-| -}
type alias Split a =
    { past : List (MixedZipper a (Branch a))
    , present : MixedZipper a (Branch a)
    , future : List (MixedZipper a (Branch a))
    }


{-| -}
split : Tree a -> Split a
split ( z, past ) =
    { present = MixedZipper.mapFocus Branch.node z
    , past = past
    , future = Branch.children z.focus
    }


{-| -}
join : Split a -> Tree a
join s =
    create
        (s.present
            |> MixedZipper.mapFocus Branch.singleton
            |> Zipper.mapFocus
                (\a -> Branch.create (Branch.node a) s.future)
        )
        s.past


{-|

    singleton 0
        |> root

        --> singleton 0

-}
root : Tree a -> Tree a
root t =
    if isRoot t then
        t

    else
        up t |> root


{-|

    import Zipper exposing (Zipper)

    fromPath (Zipper [] 0 [1, 2])
        |> leaf
        |> up
        |> focus

        --> 1

-}
leaf : Tree a -> Tree a
leaf t =
    if isLeaf t then
        t

    else
        down t |> leaf


{-| -}
left : Tree a -> Tree a
left =
    MixedNonempty.mapHead Zipper.left


{-| -}
right : Tree a -> Tree a
right =
    MixedNonempty.mapHead Zipper.right


{-| -}
leftmost : Tree a -> Tree a
leftmost =
    MixedNonempty.mapHead Zipper.leftmost


{-| -}
rightmost : Tree a -> Tree a
rightmost =
    MixedNonempty.mapHead Zipper.rightmost


{-| -}
type Walk a
    = Walk Direction (EdgeOperation a)
    | Find (a -> Bool)
    | Jump Edge


{-| -}
type Edge
    = Root
    | Leaf


{-| `Wrap`: Default operation mode.
`Insert a`: Append empty segment.
`Fail fu`: Mark focus with fu. Useful to propagate the edge error. Fail silently with fu==identity.
-}
type EdgeOperation a
    = Wrap
    | Insert (Branch a)
    | Fail (a -> a)


{-| `Left` and `Right` are trivial, whereas `Up` will add a
whole level with just a singleton `a`
-}
insert : Direction -> a -> Tree a -> Tree a
insert direction a =
    case direction of
        Left ->
            insertLeft (Branch.singleton a)

        Right ->
            insertRight (Branch.singleton a)

        Up ->
            MixedNonempty.insert (MixedZipper.singleton a)

        Down ->
            MixedNonempty.mapHead (Zipper.mapFocus (Branch.insert (MixedZipper.singleton a)))

        Here ->
            identity


{-|

    import Zipper.Branch as Branch exposing (Branch)

    tree : Tree Int
    tree =
        singleton 0
            |> growLeaf 1
            |> growLeaf 2
            |> growLeft (Branch.fromPath ( 3, [ 4, 5 ] ))

-}
go : Walk a -> Tree a -> Tree a
go w =
    case w of
        Walk Here _ ->
            identity

        Walk Left Wrap ->
            left

        Walk Left (Insert b) ->
            \t ->
                if isLeftmost t then
                    insertLeft b t |> left

                else
                    left t

        Walk Left (Fail fu) ->
            \t ->
                if isLeftmost t then
                    mapFocus fu t

                else
                    left t

        Walk Right Wrap ->
            right

        Walk Right (Insert b) ->
            \t ->
                if isRightmost t then
                    insertRight b t |> right

                else
                    right t

        Walk Right (Fail fu) ->
            \t ->
                if isRightmost t then
                    mapFocus fu t

                else
                    right t

        Walk Up Wrap ->
            up

        Walk Up (Insert r) ->
            \t ->
                if isRoot t then
                    growRoot r t |> root

                else
                    up t

        Walk Up (Fail fu) ->
            \t ->
                if isRoot t then
                    mapFocus fu t

                else
                    up t

        Walk Down Wrap ->
            down

        Walk Down (Insert r) ->
            \t ->
                if isLeaf t then
                    growBranch r t |> down

                else
                    down t

        Walk Down (Fail fu) ->
            \t ->
                if isLeaf t then
                    mapFocus fu t

                else
                    down t

        Jump Root ->
            root

        Jump Leaf ->
            leaf

        Find isHere ->
            \t ->
                let
                    myDirections =
                        zipDirections t
                            |> flatten
                            |> List.find (Tuple.second >> isHere)
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault []

                    --|> Debug.log "Found via"
                in
                Fold.list (\d -> go (Walk d Wrap)) myDirections t


{-| `foldr defold ^= identity`

    import Zipper exposing (Zipper)

    fromPath (Zipper [1, 2] 3 [4])
        |> foldr defoldr
        |> focus

        --> 3

-}
defoldr : Foldr {} a (List (Branch a)) (MixedZipper a (Branch a)) (Zipper (Branch a)) (List (MixedZipper a (Branch a))) (Branch a) (Tree a)
defoldr =
    { consAisle = (::) --: b -> aisle -> aisle
    , join = MixedZipper.create
    , joinBranch = Zipper.create -- : b -> aisle -> aisle -> zB
    , consTrunk = (::) --: z -> trunk -> trunk
    , mergeBranch = Branch.create --: a -> trunk -> b
    , mergeTree = create -- : z -> trunk -> result
    , leaf = []
    , left = []
    , right = []
    }


{-| -}
mapfold : (a -> b) -> Foldr {} a (List (Branch b)) (MixedZipper b (Branch b)) (Zipper (Branch b)) (List (MixedZipper b (Branch b))) (Branch b) (Tree b)
mapfold fu =
    { consAisle = (::) --: b -> aisle -> aisle
    , join = \a l r -> MixedZipper.create (fu a) l r -- a -> aisle -> aisle -> z
    , joinBranch = Zipper.create -- : b -> aisle -> aisle -> zB
    , consTrunk = (::) --: z -> trunk -> trunk
    , mergeBranch = fu >> Branch.create --: a -> trunk -> b
    , mergeTree = create -- : z -> trunk -> result
    , leaf = []
    , left = []
    , right = []
    }


{-| -}
any : (a -> Bool) -> Tree a -> Bool
any fu =
    flatten >> List.any fu


{-| -}
map : (a -> b) -> Tree a -> Tree b
map fu =
    foldr (mapfold fu)


{-| -}
mapByPosition : (Position -> a -> b) -> Tree a -> Tree b
mapByPosition fu =
    zipPositions
        >> map (\( pos, a ) -> fu pos a)


{-| -}
mapFocus : (a -> a) -> Tree a -> Tree a
mapFocus =
    Branch.mapNode >> mapBranch





{-| -}
mapTrace : (a -> a) -> Tree a -> Tree a
mapTrace =
    MixedZipper.mapFocus >> MixedNonempty.mapTail


{-| -}
mapSpine : (a -> a) -> Tree a -> Tree a
mapSpine fu =
    mapBranch (Branch.mapSpine fu)
        >> mapTrace fu


{-| -}
mapRoot : (a -> a) -> Tree a -> Tree a
mapRoot fu tree =
    MixedNonempty.mapLast
        (MixedZipper.map fu (Branch.map fu))
        tree
        |> (\result ->
                case result of
                    Ok ok ->
                        ok

                    Err _ ->
                        (Branch.map fu
                            |> Zipper.map
                            |> MixedNonempty.mapHead
                        )
                            tree
           )


{-| -}
mapLeaves : (a -> a) -> Tree a -> Tree a
mapLeaves fu =
    MixedNonempty.map
        (Zipper.map (Branch.mapLeaves fu))
        (MixedZipper.mapPeriphery (Branch.mapLeaves fu))


{-| maps only the leaf under the focus
-}
mapFocusedLeaf : (a -> a) -> Tree a -> Tree a
mapFocusedLeaf =
    Branch.mapFocusedLeaf >> mapBranch


{-| -}
mapRoots : (a -> a) -> Tree a -> Tree a
mapRoots fu =
    MixedNonempty.mapLast2
        (MixedZipper.map fu (Branch.mapNode fu))
        >> Result.extract
            (MixedNonempty.mapHead (Zipper.map (Branch.mapNode fu)))


{-| -}
mapBranch : (Branch a -> Branch a) -> Tree a -> Tree a
mapBranch =
    Zipper.mapFocus >> MixedNonempty.mapHead


{-| -}
mapAisles : (Branch a -> Branch a) -> Tree a -> Tree a
mapAisles =
    Zipper.mapPeriphery >> MixedNonempty.mapHead


{-| -}
mapAisleNodes : (a -> a) -> Tree a -> Tree a
mapAisleNodes =
    Branch.mapNode >> Zipper.mapPeriphery >> MixedNonempty.mapHead


{-| Deletes the focus

See [Zipper#deleteFocus](Zipper#deleteFocus) for the case handling in the
left/right dimension

  - If there is a past, go up instead of using the replacement
  - If there is no past, use the replacement

Provide a default for the case of a singleton Tree

    import Zipper.Branch as Branch

    singleton "a"
        |> deleteFocus (Branch.singleton "default")
        |> focus

        --> "default"


    import Zipper exposing (Zipper)

    fromPath (Zipper [0] 1 [2])
        |> deleteFocus (Branch.singleton 3)
        |> focus

        --> 0

    fromPath (Zipper [] 0 [1, 2])
        |> deleteFocus (Branch.singleton 3)
        |> focus

        --> 3

-}
deleteFocus : Branch a -> Tree a -> Tree a
deleteFocus default tree =
    if Zipper.isSingleton (MixedNonempty.head tree) then
        deletePresent
            (Zipper.singleton default)
            tree

    else
        MixedNonempty.mapHead
            (Zipper.deleteFocus default)
            tree


{-| See [Zipper#deleteFocus](Zipper#deleteFocus) for the case handling in the
left/right dimension.

  - If there is a past, go up instead of using the replacement

-}
deleteIfPossible : Tree a -> Tree a
deleteIfPossible tree =
    deleteFocus (focusedBranch tree) tree


deletePresent : Zipper (Branch a) -> Tree a -> Tree a
deletePresent default =
    MixedNonempty.deleteWithDefault
        default
        (MixedZipper.mapFocus Branch.singleton)


{-| -}
insertLeft : Branch a -> Tree a -> Tree a
insertLeft =
    Zipper.insertLeft >> MixedNonempty.mapHead


{-| -}
insertRight : Branch a -> Tree a -> Tree a
insertRight =
    Zipper.insertRight >> MixedNonempty.mapHead


{-| -}
consLeft : Branch a -> Tree a -> Tree a
consLeft =
    Zipper.consLeft >> MixedNonempty.mapHead


{-| -}
consRight : Branch a -> Tree a -> Tree a
consRight =
    Zipper.consRight >> MixedNonempty.mapHead


{-| -}
prepend : List (Branch a) -> Tree a -> Tree a
prepend =
    Zipper.prepend >> MixedNonempty.mapHead


{-| -}
append : List (Branch a) -> Tree a -> Tree a
append =
    Zipper.append >> MixedNonempty.mapHead


{-| -}
growLeft : Branch a -> Tree a -> Tree a
growLeft =
    Zipper.growLeft >> MixedNonempty.mapHead


{-| -}
growRight : Branch a -> Tree a -> Tree a
growRight =
    Zipper.growRight >> MixedNonempty.mapHead


{-| -}
growRoot : Branch a -> Tree a -> Tree a
growRoot =
    Branch.allGenerations
        >> Nonempty.toList
        >> (\newGens tree -> MixedNonempty.append newGens tree)


{-| -}
growRootUp : a -> Tree a -> Tree a
growRootUp =
    MixedZipper.singleton >> MixedNonempty.grow


{-| grow a branch left of the root(s). If there is no root,
-}
growRootLeft : Branch a -> Tree a -> Tree a
growRootLeft br t =
    MixedNonempty.mapLast (MixedZipper.growLeft br) t
        |> Result.withDefault (growLeft br t)


{-| -}
growRootRight : Branch a -> Tree a -> Tree a
growRootRight br t =
    MixedNonempty.mapLast (MixedZipper.growRight br) t
        |> Result.withDefault (growRight br t)


{-| -}
growLeaf : a -> Tree a -> Tree a
growLeaf =
    Branch.growLeaf >> Zipper.mapFocus >> MixedNonempty.mapHead


{-| -}
growBranch : Branch a -> Tree a -> Tree a
growBranch =
    Branch.growBranch >> Zipper.mapFocus >> MixedNonempty.mapHead



---- DECONSTRUCT ----


{-| omits the vertical position in the Tree

    import Zipper exposing (Zipper)
    import Zipper.Branch as Branch

    petrify (fromPath (Zipper [0] 1 [2]))
        |> Zipper.focus
        |> Branch.path

        --> ( 0, [1, 2] )

-}
petrify : Tree a -> Zipper (Branch a)
petrify =
    root >> MixedNonempty.head


{-| -}
focusedBranch : Tree a -> Branch a
focusedBranch =
    MixedNonempty.head >> Zipper.focus


{-|

    focus (singleton 1) --> 1

-}
focus : Tree a -> a
focus =
    MixedNonempty.head >> Zipper.focus >> Branch.node


{-| -}
getAisleNodes : Tree a -> Zipper a
getAisleNodes =
    MixedNonempty.head >> Zipper.map Branch.node


{-| -}
getRoot : Tree a -> a
getRoot =
    MixedNonempty.last
        >> Result.unpack
            (Zipper.focus >> Branch.node)
            MixedZipper.focus


{-| -}
getLeftmostRoot : Tree a -> a
getLeftmostRoot =
    MixedNonempty.last
        >> Result.unpack
            (Zipper.getLeftmost >> Branch.node)
            (MixedZipper.getLeftmost >> Result.unpack identity Branch.node)


{-| -}
getRightmostRoot : Tree a -> a
getRightmostRoot =
    MixedNonempty.last
        >> Result.unpack
            (Zipper.getRightmost >> Branch.node)
            (MixedZipper.getRightmost >> Result.unpack identity Branch.node)


{-| -}
circumference : Tree a -> Int
circumference =
    MixedNonempty.head >> Zipper.length


{-| -}
isLeftmost : Tree a -> Bool
isLeftmost =
    MixedNonempty.head >> Zipper.isLeftmost


{-| -}
isRightmost : Tree a -> Bool
isRightmost =
    MixedNonempty.head >> Zipper.isRightmost


{-| -}
isRoot : Tree a -> Bool
isRoot =
    MixedNonempty.tail >> (==) []


{-| -}
isLeaf : Tree a -> Bool
isLeaf =
    MixedNonempty.head >> Zipper.focus >> Branch.isLeaf


{-| -}
flatten : Tree a -> List a
flatten =
    fold
        { init = identity --: b -> c
        , branch = Branch.flatFold --: Branch.Fold f a b
        , grow =
            --:
            { leftwards = (++) --: b -> c -> c
            , rightwards = (++) --: b -> c -> c
            , upwards = (::) --: a -> c -> c
            }
        }


{-| -}
toDict : (a -> ( comparable, v )) -> Tree a -> Dict comparable v
toDict fu =
    flatten >> List.map fu >> Dict.fromList


{-| a simple Fold starting at the focus.

  - `branch` folds any sub-branch
  - `grow.leftwards` and `grow.rightwards` fold the aisles
  - `grow.upwards` folds just the breadcrumbs.

Each function here modifes a `c`, so that type needs to be quite flexible.

-}
type alias Fold f a b c =
    { f
        | init : b -> c
        , branch : Branch.Fold f a b
        , grow :
            { leftwards : b -> c -> c
            , rightwards : b -> c -> c
            , upwards : a -> c -> c
            }
    }


{-| -}
defold : Fold {} a (Branch a) (Tree a)
defold =
    { init = fromBranch
    , branch = Branch.defold
    , grow =
        { leftwards = growRootLeft
        , rightwards = growRootRight
        , upwards = growRootUp
        }
    }


type alias PosTree a =
    Tree ( Position, a )


zipPositions : Tree a -> PosTree a
zipPositions =
    zipDirections
        >> map (Tuple.mapFirst (\dirs -> Fold.directionsToRole dirs |> (\r -> { role = r, isRoot = False, isLeaf = False, path = dirs })))
        >> mapLeaves (Tuple.mapFirst (\pos -> { pos | isLeaf = True }))
        >> mapRoots (Tuple.mapFirst (\pos -> { pos | isRoot = True }))


{-| -}
type alias DirTree a =
    Tree ( List Direction, a )


{-|

    fromPath (0, [1, 2])
        |> zipDirections
        |> path

        -- (([], 0), [(Down, 1), (Down, 2)])

-}
zipDirections : Tree a -> DirTree a
zipDirections =
    fold defoldWithDirections


{-| -}
defoldWithDirections : Fold {} a (Branch.DirBranch a) (DirTree a)
defoldWithDirections =
    let
        withDirection :
            Direction
            -> (DirTree a -> ( List Direction, a ))
            -> (Branch.DirBranch a -> DirTree a -> DirTree a)
            -> Branch.DirBranch a
            -> (DirTree a -> DirTree a)
        withDirection dir getReferenceNode build newBranch oldTree =
            let
                accumulatedPath : List Direction
                accumulatedPath =
                    Tuple.first (getReferenceNode oldTree)

                newPath : List Direction -> List Direction
                newPath =
                    \subPath ->
                        accumulatedPath
                            ++ dir
                            :: subPath
            in
            Branch.map (Tuple.mapFirst newPath) newBranch
                |> (<|) build
                |> (|>) oldTree
    in
    { init = defold.init
    , branch = Branch.defoldWithDirections
    , grow =
        { leftwards = withDirection Left getLeftmostRoot defold.grow.leftwards
        , rightwards = withDirection Right getRightmostRoot defold.grow.rightwards
        , upwards =
            \newNode oldTree ->
                Tuple.first (getRoot oldTree)
                    ++ [ Up ]
                    |> (<|) Tuple.pair
                    |> (|>) newNode
                    |> (<|) defold.grow.upwards
                    |> (|>) oldTree
        }
    }


{-| The Fold of a Tree _is_

The Fold of a MixedNonempty
where
the Head is a Zipper (Branch a)
and the Tail consists of MixedZipper a (Branch a).

How MixedNonempty.fold works:

1.  Initialize by feeding the head to f.init
2.  Along the tail, successively apply f.grow

How Zipper.fold and MixedZipper.fold work:

1.  Initialize by feeding the focus to f.init
2.  Along the left aisle, apply f.grow.leftwards
3.  Along the right aisle, apply f.grow.rightwards

-}
fold : Fold f a b c -> Tree a -> c
fold f =
    let
        headFold : Zipper.Fold {} (Branch a) c
        headFold =
            { init =
                -- : Branch a -> c
                Branch.fold f.branch >> f.init
            , grow =
                -- : { leftwards : Branch a -> c -> c, rightwards : Branch a -> c -> c }}
                { leftwards = Branch.fold f.branch >> f.grow.leftwards
                , rightwards = Branch.fold f.branch >> f.grow.rightwards
                }
            }

        tailFold : c -> MixedZipper.Fold {} a (Branch a) c
        tailFold c =
            { init =
                -- : a -> c
                \a -> f.grow.upwards a c
            , grow =
                -- : { leftwards : Branch a -> c -> c, rightwards : Branch a -> c -> c }}
                { leftwards = Branch.fold f.branch >> f.grow.leftwards
                , rightwards = Branch.fold f.branch >> f.grow.rightwards
                }
            }
    in
    MixedNonempty.fold
        { init =
            --: h -> c     ---- (Zipper (Branch a)) -> c
            Zipper.fold headFold
        , grow =
            -- a -> c -> c ---- (MixedZipper a (Branch a)) -> c -> c
            \generation c -> MixedZipper.fold (tailFold c) generation
        }


{-|

    import Zipper exposing (Zipper)
    import Zipper.Mixed as MixedZipper
    import Zipper.Branch as Branch
    import Nonempty.Mixed as MixedNonempty

    Zipper [-1] 0 [1, 2, 3, 4]
        |> fromPath
        |> foldr defoldr
        |> path

        --> Zipper [-1] 0 [1, 2, 3, 4]

    Zipper [20] 30 [40, 2, 3, 4]
        |> fromPath
        |> foldr
            { consAisle   = min
            , join        = \a l r -> min a (min l r)
            , joinBranch  = \a l r -> min a (min l r)                  --: b -> aisle -> aisle -> zB
            , consTrunk   = min                         --: z -> trunk -> trunk
            , mergeBranch = min                  --: a -> trunk -> b
            , mergeTree   = min                         --: zB -> trunk -> result
            , leaf        = 2^31 - 1
            , left        = 2^31 - 1
            , right       = 2^31 - 1
        }

        --> 2

-}
foldr :
    { f
        | consAisle : b -> aisle -> aisle
        , join : a -> aisle -> aisle -> z
        , joinBranch : b -> aisle -> aisle -> zB
        , consTrunk : z -> trunk -> trunk
        , mergeBranch : a -> trunk -> b
        , mergeTree : zB -> trunk -> result
        , leaf : trunk
        , left : aisle
        , right : aisle
    }
    -> Tree a
    -> result
foldr f =
    MixedNonempty.foldr
        { cons =
            \zipper trunk ->
                f.consTrunk
                    (MixedZipper.foldr
                        { cons = Branch.foldr f >> f.consAisle
                        , join = f.join
                        , init =
                            { left = f.left
                            , right = f.right
                            }
                        }
                        zipper
                    )
                    trunk
        , merge =
            \future past ->
                f.mergeTree
                    (Zipper.foldr
                        { cons = Branch.foldr f >> f.consAisle
                        , join = Branch.foldr f >> f.joinBranch
                        , left = f.left
                        , right = f.right
                        }
                        future
                    )
                    past
        , leaf = f.leaf
        }



{- fold from inside to outside.
   foldl :
       { f
           | initPeriphery : trunk -> a -> (aisle, aisle)
           , initTrunk : zB -> trunk
           , consAisle : b -> aisle -> aisle
           , join : (aisle, aisle) -> z
           , consTrunk : z -> trunk
           , root : a -> trunk
           , mergeBranch : trunk -> b

           , join : a -> aisle -> aisle -> z
           , joinBranch : b -> aisle -> aisle -> zB
           , consTrunk : z -> trunk -> trunk
           , mergeBranch : a -> trunk -> b
           , mergeTree : zB -> trunk -> result
       }
       -> Tree a
       -> result
   foldl f =
       MixedNonempty.foldl
           { cons = \zipper trunk ->
               f.consTrunk
                   ( MixedZipper.foldl
                       { cons = f.consAisle
                       , join = f.join
                       , init = f.initPeriphery trunk
                       --  cons : b -> acc -> acc
                       --, join : (acc, acc) -> result
                       --, init : focus -> (acc, acc)
                       } zipper
                   )

               cons =
               \zipper trunk ->
                   f.consTrunk
                       (MixedZipper.foldr
                           { cons = Branch.foldr f >> f.consAisle
                           , join = f.join
                           , left = f.left
                           , right = f.right
                           }
                           zipper
                       )
                       trunk
           , merge =
               \future past ->
                   f.mergeTree
                       (Zipper.foldr
                           { cons = Branch.foldr f >> f.consAisle
                           , join = Branch.foldr f >> f.joinBranch
                           , left = f.left
                           , right = f.right
                           }
                           future
                       )
                       past
           , init = f.initTrunk
           }
-}


{-| -}
type ViewMode f a msg viewmodel aisle z zB trunk b c html
    = Uniform (Fold f a b c) { toHtml : c -> html }


{-| -}
view : ViewMode f a msg viewmodel aisle z zB trunk b c html -> Tree a -> html
view viewMode =
    case viewMode of
        Uniform f config ->
            fold defold >> fold f >> config.toHtml


{-| -}
flatMap : (a -> b) -> Tree a -> List b
flatMap fu =
    map fu >> flatten
