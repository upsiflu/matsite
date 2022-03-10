module Zipper.Tree exposing
    ( Tree, singleton, animate, fromPath
    , left, leftmost
    , right, rightmost
    , up, down
    , root, leaf
    , map, mapFocus, mapBranch
    , insertLeft, prepend
    , insertRight, append
    , growRoot, growLeaf, growBranch
    , deleteFocus
    , insert
    , focus
    , path
    , circumference
    , petrify
    , fold, defold
    , view
    )

{-| A List of branches 🌿 that can be navigated horizontally and vertically.

  - When walking left or right, you will wrap silently because we assume that the branches are set in a circle.
  - By convention, the `root`s are on top

@docs Tree, singleton, animate, fromPath


## Navigate

@docs left, leftmost
@docs right, rightmost
@docs up, down
@docs root, leaf


## Map

@docs map, mapFocus, mapBranch


## Insert

@docs insertLeft, prepend
@docs insertRight, append
@docs insert


## Delete

@docs deleteFocus


## Grow

@docs growRoot, growLeaf, growBranch


## Deconstruct

@docs focus
@docs path
@docs circumference
@docs petrify
@docs fold, defold


## View

@docs view

-}

import Nonempty exposing ( Nonempty)
import Nonempty.Mixed exposing (MixedNonempty)
import Zipper.Mixed exposing (MixedZipper)
import Zipper exposing (Zipper)
import Zipper.Branch as Branch exposing (Branch)
import Fold exposing (Fold)


import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (onClick)


{-| The Zipper Tree has a head (the several branching lower levels) as well as a tail (the list of upper levels, each having a focus and perhaps alternative branches to go down into).
Its focus is the node of the branch that is focused in the zipper that is the Tree's head.-}
type alias Tree a =
    MixedNonempty
        (Zipper (Branch a))
        (MixedZipper a (Branch a))



---- CREATE ----


{-| -}
singleton : a -> Tree a
singleton =
    Branch.singleton >> Zipper.singleton >> Nonempty.Mixed.singleton

{-|-}
fromBranch : Branch a -> Tree a 
fromBranch =
    Zipper.singleton >> animate

{-| creates an oblong Tree -}
fromPath : Zipper a -> Tree a
fromPath z =
    let 
        createLevel : a -> MixedZipper a (Branch a)
        createLevel =
            Branch.singleton 
                >> Zipper.Mixed.singleton 
                >> Zipper.Mixed.deviateBy Branch.node 
    in
    join
        { past = List.map createLevel z.left
        , present = createLevel z.focus
        , future = List.map createLevel z.right
        }

{-| reads all focused values, from past to future -}
path : Tree a -> Zipper a
path =
    split
        >> \s->
            Zipper 
                (List.map Zipper.Mixed.focus s.past)
                (Zipper.Mixed.focus s.present)
                (List.map Zipper.Mixed.focus s.future)



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
    Nonempty.Mixed.singleton

{-| internal representation -}
merge : Zipper (Branch a) -> List (MixedZipper a (Branch a)) -> Tree a
merge = 
    Nonempty.Mixed.merge



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
    split >>
        \s-> join <|
            case (s.past, s.future) of
                -- no past:
                ( [], future )  ->
                    case List.reverse future of
                        [] -> s
                        e::rutuf ->
                            { past = rutuf++[s.present]
                            , present = e
                            , future = []
                            }
                -- there is past:
                ( p::ast, future )  ->
                    { past = ast
                    , present = p
                    , future = s.present::future
                    }


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
    split >>
        \s-> join <|
            case (s.past, s.future) of
                -- no future:
                ( past, [] )  ->
                    case List.reverse past of
                        [] -> s
                        t::sap ->
                            { past = []
                            , present = t
                            , future = sap++[s.present]
                            }
                -- there is future:
                ( past, f::uture )  ->
                    { past = s.present::past
                    , present = f
                    , future = uture
                    }


{-|-}
type alias Split a =
    { past : List (MixedZipper a (Branch a))
    , present : MixedZipper a (Branch a)
    , future : List (MixedZipper a (Branch a))
    }


{-|-}
split : Tree a -> Split a
split (z, past) =
    { present = 
        Zipper.Mixed.fromZipper z
            |> Zipper.Mixed.deviateBy Branch.node
    , past = past
    , future = Branch.children z.focus
    }


{-|-}
join : Split a -> Tree a
join s =
    merge
    ( s.present 
        |> Zipper.Mixed.homogenize  --Zipper without future
        |> Zipper.Mixed.toZipper
        |> Zipper.mapFocus
            (\a -> Branch.merge ( Branch.node a ) s.future )
    )
    s.past








{-|

    singleton 0
        |> root

        --> singleton 0

-}
root : Tree a -> Tree a
root t =
    if isRoot t then t else up t |> root

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
    if isLeaf t then t else down t |> leaf


{-| -}
left : Tree a -> Tree a
left =
    Nonempty.Mixed.mapHead Zipper.left


{-| -}
right : Tree a -> Tree a
right =
    Nonempty.Mixed.mapHead Zipper.right


{-| -}
leftmost : Tree a -> Tree a
leftmost =
    Nonempty.Mixed.mapHead Zipper.leftmost


{-| -}
rightmost : Tree a -> Tree a
rightmost =
    Nonempty.Mixed.mapHead Zipper.rightmost

{-| -}
type Direction
    = Left 
    | Right
    | Up
    | Down
    | Here

type Walk a
    = Walk Direction (EdgeOperation a)
    | Find ( a -> Float )

{-| 

`Wrap`: Default operation mode.
`Insert a`: Append empty segment.
`Fail fu`: Mark focus with fu. Useful to propagate the edge error. Fail silently with fu==identity.
-}
type EdgeOperation a
    = Wrap
    | Insert (Branch a)
    | Fail ( a -> a )

anyways = Fail identity

{-| -}
walk : Walk a -> Tree a -> Tree a
walk w =
    case w of
        Walk Here _ -> identity

        Walk Left Wrap -> left
        Walk Left (Insert b) ->
            (\t -> if isLeftmost t then insertLeft b t |> left else left t)
        Walk Left (Fail fu) ->
            (\t -> if isLeftmost t then mapFocus fu t else left t )

        Walk Right Wrap -> right
        Walk Right (Insert b) ->
            (\t -> if isRightmost t then insertRight b t |> right else right t)
        Walk Right (Fail fu) ->
            (\t -> if isRightmost t then mapFocus fu t else right t)

        Walk Up Wrap -> up
        Walk Up (Insert r) -> 
            (\t -> if isRoot t then growRoot r t |> root else up t)
        Walk Up (Fail fu) ->
            (\t -> if isRoot t then mapFocus fu t else up t)
        
        Walk Down Wrap -> down
        Walk Down (Insert r) -> 
            (\t -> if isLeaf t then growBranch r t |> down else down t)
        Walk Down (Fail fu) ->
            (\t -> if isLeaf t then mapFocus fu t else down t)

        Find howToScore ->

            identity -- TODO!


{-| `fold defold ^= identity`

    import Zipper exposing (Zipper)

    fromPath (Zipper [1, 2] 3 [4])
        |> fold defold
        |> focus

        --> 3

-}
defold : Fold {} a (List (Branch a)) (MixedZipper a (Branch a)) (Zipper (Branch a)) (List (MixedZipper a (Branch a))) (Branch a) (Tree a)
defold =
    { consAisle = (::) --: b -> aisle -> aisle
    , join = (\a l r -> Zipper.Mixed.join (Branch.singleton a) l r Branch.node)
    , joinBranch = Zipper.join -- : b -> aisle -> aisle -> zB
    , consTrunk = (::) --: z -> trunk -> trunk
    , mergeBranch = Branch.merge --: a -> trunk -> b
    , mergeTree = merge -- : z -> trunk -> result
    , leaf = []
    , left = []
    , right = []
    }

{-| `fold defold ^= identity`

    import Zipper exposing (Zipper)

    fromPath (Zipper [1, 2] 3 [4])
        |> fold defold
        |> focus

        --> 3

-}
mapfold : (a->b) -> Fold {} a (List (Branch b)) (MixedZipper b (Branch b)) (Zipper (Branch b)) (List (MixedZipper b (Branch b))) (Branch b) (Tree b)
mapfold fu =
    { consAisle = (::) --: b -> aisle -> aisle
    , join = (\a l r -> Zipper.Mixed.join (Branch.singleton (fu a)) l r Branch.node) -- a -> aisle -> aisle -> z
    , joinBranch = Zipper.join -- : b -> aisle -> aisle -> zB
    , consTrunk = (::) --: z -> trunk -> trunk
    , mergeBranch = fu >> Branch.merge --: a -> trunk -> b
    , mergeTree = merge -- : z -> trunk -> result
    , leaf = []
    , left = []
    , right = []
    }

{-| -}
map : (a -> b) -> Tree a -> Tree b
map fu =
    fold (mapfold fu)


{-| -}
mapFocus : (a -> a) -> Tree a -> Tree a
mapFocus =
    Branch.map >> mapBranch


{-| -}
mapBranch : (Branch a -> Branch a) -> Tree a -> Tree a
mapBranch =
    Zipper.mapFocus >> Nonempty.Mixed.mapHead


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
    if Zipper.isSingleton ( Nonempty.Mixed.head tree )
    then 
        deletePresent 
            (Zipper.singleton default)
            tree
    else
        Nonempty.Mixed.mapHead
            (Zipper.deleteFocus default) tree


deletePresent : Zipper (Branch a) -> Tree a -> Tree a
deletePresent default =
    let 
        pastToPresent : MixedZipper a (Branch a) ->  Zipper (Branch a)
        pastToPresent =
            Zipper.Mixed.homogenize
                >> Zipper.Mixed.toZipper
    in
    Nonempty.Mixed.delete
        default
        pastToPresent
        


{-| -}
insertLeft : Branch a -> Tree a -> Tree a
insertLeft =
    Zipper.insertLeft >> Nonempty.Mixed.mapHead


{-| -}
insertRight : Branch a -> Tree a -> Tree a
insertRight =
    Zipper.insertRight >> Nonempty.Mixed.mapHead

{-| -}
insert : Branch a -> Tree a -> Tree a
insert =
    Zipper.insert >> Nonempty.Mixed.mapHead


{-| -}
prepend : List (Branch a) -> Tree a -> Tree a
prepend =
    Zipper.prepend >> Nonempty.Mixed.mapHead


{-| -}
append : List (Branch a) -> Tree a -> Tree a
append =
    Zipper.append >> Nonempty.Mixed.mapHead

{-| -}
growRoot : Branch a -> Tree a -> Tree a
growRoot =
    Branch.allGenerations
        >> Nonempty.toList
        >> (\newGens tree -> Nonempty.Mixed.appendMany newGens tree)

{-| -}
growLeaf : a -> Tree a -> Tree a
growLeaf =
    Branch.growLeaf >> Zipper.mapFocus >> Nonempty.Mixed.mapHead
    
{-| -}
growBranch : Branch a -> Tree a -> Tree a
growBranch =
    Branch.growBranch >> Zipper.mapFocus >> Nonempty.Mixed.mapHead


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
    root >> Nonempty.Mixed.head


{-|

    focus (singleton 1) --> 1

-}
focus : Tree a -> a
focus =
    Nonempty.Mixed.head >> Zipper.focus >> Branch.node



{-| -}
circumference : Tree a -> Int
circumference =
    Nonempty.Mixed.head >> Zipper.length

{-| -}
isLeftmost : Tree a -> Bool
isLeftmost =
    Nonempty.Mixed.head >> Zipper.isLeftmost

{-| -}
isRightmost : Tree a -> Bool
isRightmost =
    Nonempty.Mixed.head >> Zipper.isRightmost

{-| -}
isRoot : Tree a -> Bool
isRoot =
    Nonempty.Mixed.tail >> (==) []

{-| -}
isLeaf : Tree a -> Bool
isLeaf = 
    Nonempty.Mixed.head >> Zipper.focus >> Branch.isLeaf
    
{-|-}
fold : 
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
fold f =
    Nonempty.Mixed.fold
        { cons = 
            (\zipper trunk ->
                f.consTrunk
                    ( Zipper.Mixed.fold 
                        { cons = Branch.fold f >> f.consAisle
                        , join = f.join
                        , left = f.left
                        , right = f.right
                        } 
                        zipper 
                    ) trunk
            )
        , merge =
            (\future past ->
                f.mergeTree
                    ( Zipper.fold
                        { cons = Branch.fold f >> f.consAisle
                        , join = Branch.fold f >> f.joinBranch
                        , left = f.left
                        , right = f.right
                        }
                        future 
                    )
                    past
            )
        , leaf = f.leaf
        }

{-|-}
view : ( a -> Html msg ) -> Tree a -> Html msg
view fu =
    map fu
        >> fold viewFolder
    
viewFolder :
    Fold 
        {}                --f 
        (Html msg)        --a 
        (List (Html msg)) --aisle 
        (Html msg)        --z 
        (Html msg)        ---zB 
        (List (Html msg)) --trunk 
        (Html msg)        --b 
        (Html msg)        --e
viewFolder =
    let
        focused = css [border3 (px 5) solid (rgb 120 120 120) ]
        horizontal = css [displayFlex]
    in
    { consAisle = (::) --: b -> aisle -> aisle
    , join = 
        (\a l r -> 
            Html.div [] [Html.span [] l, Html.span [focused] [ a ], Html.span [] r ]
        ) -- a -> aisle -> aisle -> z --past
    , joinBranch = 
        (\branch l r -> 
            Html.div [horizontal] [Html.div [] l, Html.div [focused] [ branch ], Html.div [] r ]
        ) -- : b -> aisle -> aisle -> zB -- future
    , consTrunk = (::) --: z -> trunk -> trunk
    , mergeBranch = 
        (\node body -> 
            Html.div [] [ Html.h1 [] [Html.text "BRANCH"], Html.div [] [node], Html.hr [] [], Html.div [] body]
        ) --: a -> trunk -> b
    , mergeTree = 
        (\future past -> 
            Html.div [] [ Html.h1 [] [Html.text "TREE"], Html.div [] past, Html.hr [] [], Html.div [] [future]]
        ) -- : z -> trunk -> result
    , leaf = []
    , left = []
    , right = []
    }


-- SCOPING --


type Scope
    = Row
    | Neighborhood Int
    | Stem



-- and many more, possibly
