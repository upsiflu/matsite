module Zipper exposing
    ( Zipper, singleton, path, join
    , focus
    , flat, fold
    , left, leftmost
    , right, rightmost
    , map, mapFocus
    , deleteFocus
    , insertLeft, prepend
    , insertRight, append
    , insert
    , length
    , isLeftmost, isRightmost, isSingleton
    )

{-|



@docs Zipper, singleton, path, join
@docs focus



## Navigate

@docs left, leftmost
@docs right, rightmost


## Map

@docs map, mapFocus


## Delete and Insert

@docs deleteFocus
@docs insertLeft, prepend
@docs insertRight, append
@docs insert


## Deconstruct

@docs flat
@docs isLeftmost, isRightmost, isSingleton
@docs length


## Fold

@docs fold

-}


{-| -}
type alias Zipper a =
    { left : List a
    , focus : a
    , right : List a
    }


{-| -}
singleton : a -> Zipper a
singleton a =
    Zipper [] a []

{-|-}
join : a -> List a -> List a -> Zipper a
join a l r =
    Zipper l a r

{-| -}
path : a -> List a -> Zipper a
path =
    Zipper []


{-|

    focus ( singleton 1 ) --> 1

-}
focus : Zipper a -> a
focus =
    .focus


{-| -}
flat : Zipper a -> List a
flat z =
    z.focus :: z.left
        |> List.reverse
        |> (++) z.right



{-| 

    singleton "x"
        |> insertRight "Y"
        |> insertRight "Z"
        |> insertRight "A"
        |> rightmost
        |> focus

        --> "Y"

    
    singleton "x"
        |> insertRight "Y"
        |> insertRight "Z"
        |> insertRight "A"
        |> rightmost
        |> length

        --> 4

-}
rightmost : Zipper a -> Zipper a
rightmost z =
    case List.reverse z.right of
        [] ->
            z

        t :: hgir ->
            { z | left = hgir++[z.focus]++z.left, focus = t, right = []}


{-|

    singleton "x"
        |> insertLeft "Y"
        |> left
        |> focus

        --> "Y"

    singleton "x"
        |> insertRight "Y"
        |> left
        |> focus

        --> "Y"

    singleton "x"
        |> insertRight "R"
        |> insertLeft "LL"
        |> insertLeft "L"
        |> length

        --> 4

-}
left : Zipper a -> Zipper a
left z =
    case z.left of
        l :: eft ->
            { z | left = eft, focus = l, right = z.focus :: z.right }

        [] ->
            rightmost z


{-| -}
leftmost : Zipper a -> Zipper a
leftmost z =
    case List.reverse z.left of
        [] ->
            z

        t :: fel ->
            { z | left = [], focus = t, right = fel ++ [z.focus] ++ z.right }


{-|

    right (singleton "x") |> focus --> "x"

-}
right : Zipper a -> Zipper a
right z =
    case z.right of
        r :: ight ->
            { z | left = z.focus :: z.left, focus = r, right = ight }

        [] ->
            leftmost z


{-|

    singleton "x"
        |> map String.toUpper

        --> singleton "X"

-}
map : (a -> b) -> Zipper a -> Zipper b
map fu z =
    { left = List.map fu z.left
    , focus = fu z.focus
    , right = List.map fu z.right
    }


{-| -}
mapFocus : (a -> a) -> Zipper a -> Zipper a
mapFocus fu z =
    { z | focus = fu z.focus }



{-| Removes the focused segment

- go left if there is a segment left
- otherwise go right if there is a segment right
- otherwise replace the focus with a default

    deleteFocus "default" (singleton "X")

    --> singleton "default"

    singleton "X"
        |> insertRight "Y" 
        |> deleteFocus "default"

    --> singleton "Y"

    singleton "X"
        |> insertLeft "W" 
        |> deleteFocus "default"

    --> singleton "W"

    singleton "X"
        |> insertLeft "W" 
        |> insertRight "Y" 
        |> deleteFocus "default"
        |> focus

    --> "W"
-}
deleteFocus : a -> Zipper a -> Zipper a
deleteFocus default z =
    case (z.left, z.right) of
        ([], []) ->
            singleton default
        (l::eft, _) ->
            { z | left = eft, focus = l }
        ([], r::ight) ->
            { z | right = ight, focus = r }

{-|

    singleton 5
        |> prepend [0, 1, 2, 3, 4]
        |> leftmost >> focus

        --> 0

-}
prepend : List a -> Zipper a -> Zipper a
prepend s z =
    { z | left = z.left ++ List.reverse s }


{-|

    singleton 0
        |> append [1, 2, 3, 4, 5]
        |> rightmost >> focus

        --> 5

-}
append : List a -> Zipper a -> Zipper a
append s z =
    { z | right = z.right ++ s }


{-| -}
insertLeft : a -> Zipper a -> Zipper a
insertLeft a z =
    { z | left = a :: z.left }


{-| -}
insertRight : a -> Zipper a -> Zipper a
insertRight a z =
    { z | right = a :: z.right }


{-| -}
insert : a -> Zipper a -> Zipper a
insert x =
    insertRight x >> right


{-| -}
length : Zipper a -> Int
length z =
    List.length z.left + List.length z.right + 1

{-|-}
isLeftmost : Zipper a -> Bool
isLeftmost z =
    z.left == []

{-|-}
isRightmost : Zipper a -> Bool
isRightmost z =
    z.right == []

{-|-}
isSingleton : Zipper a -> Bool
isSingleton z =
    isRightmost z && isLeftmost z


---- Folding
    

{-| 
-}
fold : 
    { f
    | cons : a -> acc -> acc
    , join : a -> acc -> acc -> result
    , left : acc
    , right : acc
    }
    -> Zipper a
    -> result
fold f z =
    f.join z.focus
        (List.foldr f.cons f.left z.left)
        (List.foldr f.cons f.right z.right)

