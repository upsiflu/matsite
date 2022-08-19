module Article exposing
    ( Article
    , defaultIllustration
    , empty, singleton
    , Action(..)
    , actionCodec
    , apply
    , Orientation(..), Shape(..)
    , BodyChoice(..), InfoChoice(..), Width(..)
    , initialTemplates
    , Templates, BodyTemplate(..), InfoTemplate(..)
    , toggleTemplates
    , hint, orientationToString, hasBody, isBackground, isIllustration, width, orientation
    , bodyTypeToString, infoTypeToString
    , structureClass
    , templatesAreOn
    , getTemplate
    )

{-| _To render (view|edit) Articles and format them based on their position in the tree
and on the screen, use [`Accordion.Segment`](Accordion.Segment)_

@docs Article


# Create

@docs defaultIllustration
@docs empty, singleton


## Actions

@docs Action
@docs actionCodec
@docs apply


## Field types

@docs Orientation, Shape
@docs BodyChoice, InfoChoice, Width


## Template types

@docs initialTemplates
@docs Templates, BodyTemplate, InfoTemplate


# Modify

@docs toggleTemplates


# Deconstruct

@docs hint, orientationToString, hasBody, isBackground, isIllustration, width, orientation

@docs bodyTypeToString, infoTypeToString

@docs structureClass

@docs templatesAreOn


# Query

@docs getTemplate

-}

import Article.Fab as Fab exposing (Fab(..))
import Codec exposing (Codec, bool, field, string)
import Css exposing (..)
import Dict exposing (Dict)
import Directory exposing (Directory)
import Fold exposing (Direction(..), Role(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (..)
import Layout exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Occurrence
import Time exposing (Zone)


{-| only contains user-editable properties
-}
type alias Article =
    { id : String
    , caption : { text : String, showsDate : Bool }
    , info : Maybe InfoChoice
    , body : BodyChoice
    , shape : Shape
    , fab : Maybe Fab
    , additionalClasses : List String
    }


{-| Actions are encoded and decoded
-}
type Action
    = WithCaption { text : String, showsDate : Bool }
    | WithInfo (Maybe InfoChoice)
    | WithBody BodyChoice
    | WithShape Shape
    | WithFab (Maybe Fab)
    | WithClasses (List String)


{-| -}
actionCodec : Codec Action
actionCodec =
    Codec.custom
        (\cap inf bod sha fab_ cla value ->
            case value of
                WithCaption c ->
                    cap c

                WithInfo i ->
                    inf i

                WithBody b ->
                    bod b

                WithShape s ->
                    sha s

                WithFab f ->
                    fab_ f

                WithClasses c ->
                    cla c
        )
        |> Codec.variant1 "WithCaption"
            WithCaption
            (Codec.object Caption
                |> field "text" .text string
                |> field "showsDate" .showsDate bool
                |> Codec.buildObject
            )
        |> Codec.variant1 "WithInfo" WithInfo (infoCodec |> Codec.maybe)
        |> Codec.variant1 "WithBody" WithBody bodyCodec
        |> Codec.variant1 "WithShape" WithShape shapeCodec
        |> Codec.variant1 "WithFab" WithFab (Fab.codec |> Codec.maybe)
        |> Codec.variant1 "WithClasses" WithClasses (Codec.map (String.split " ") (String.join " ") Codec.string)
        |> Codec.buildCustom


{-| -}
type Width
    = Columns Int
    | Screen


{-| -}
widthCodec : Codec Width
widthCodec =
    Codec.custom
        (\col scr value ->
            case value of
                Columns i ->
                    col i

                Screen ->
                    scr
        )
        |> Codec.variant1 "Columns" Columns Codec.int
        |> Codec.variant0 "Screen" Screen
        |> Codec.buildCustom


type alias Caption =
    { text : String, showsDate : Bool }


type alias Heading =
    Maybe String


{-| -}
type Shape
    = Oriented Orientation Width
    | Background


{-| -}
shapeCodec : Codec Shape
shapeCodec =
    Codec.custom
        (\oriented background value ->
            case value of
                Oriented o w ->
                    oriented o w

                Background ->
                    background
        )
        |> Codec.variant2 "Oriented" Oriented orientationCodec widthCodec
        |> Codec.variant0 "Background" Background
        |> Codec.buildCustom


{-| -}
orientationCodec : Codec Orientation
orientationCodec =
    Codec.map (orientationFromString >> Maybe.withDefault Vertical) orientationToString Codec.string


{-| -}
width : Article -> Width
width s =
    case s.shape of
        Background ->
            Screen

        Oriented _ w ->
            w


{-| option "Preset" is grayed out if templates.info==Nothing
-}
type InfoChoice
    = CustomByline Int
    | CustomToc


infoCodec : Codec InfoChoice
infoCodec =
    Codec.custom
        (\cbyl ctoc value ->
            case value of
                CustomByline i ->
                    cbyl i

                CustomToc ->
                    ctoc
        )
        |> Codec.variant1 "CustomByline" CustomByline Codec.int
        |> Codec.variant0 "CustomToc" CustomToc
        |> Codec.buildCustom


{-| -}
type InfoTemplate
    = Byline Int (Html Never)
    | Toc


{-| -}
type BodyChoice
    = PeekThrough
    | CustomContent Heading
    | CustomIllustration


bodyCodec : Codec BodyChoice
bodyCodec =
    Codec.custom
        (\pee con ill value ->
            case value of
                PeekThrough ->
                    pee

                CustomContent h ->
                    con h

                CustomIllustration ->
                    ill
        )
        |> Codec.variant0 "PeekThrough" PeekThrough
        |> Codec.variant1 "CustomContent" CustomContent (Codec.maybe string)
        |> Codec.variant0 "CustomIllustration" CustomIllustration
        |> Codec.buildCustom


{-| -}
type BodyTemplate
    = Content Heading (Directory -> Html Never)
    | Illustration (Directory -> Html Never)


{-| akin to update, but with serializable `Action` instead of `Msg`
-}
apply : Action -> Article -> Article
apply a s =
    case a of
        WithCaption c ->
            { s | caption = c }

        -- Content
        WithBody b ->
            { s | body = b }

        WithInfo i ->
            { s | info = i }

        WithFab f ->
            { s | fab = f }

        WithClasses c ->
            { s | additionalClasses = c }

        WithShape h ->
            { s | shape = h }


{-| per-session switchable hardcoded presets; off by default
-}
type alias Templates =
    { body : Dict String ( Bool, BodyTemplate ), info : Dict String ( Bool, InfoTemplate ) }


{-| returns a corresponding template if it's activated
-}
getTemplate : (Templates -> Dict String ( Bool, v )) -> Article -> Templates -> Maybe v
getTemplate selector s =
    selector
        >> Dict.get s.id
        >> Maybe.andThen
            (\( activated, t ) ->
                if activated then
                    Just t

                else
                    Nothing
            )


{-| Volatile per-session preset dicts; individually switchable; all off by default; see `Data` module for presets
-}
initialTemplates : { body : Dict String ( Bool, BodyTemplate ), info : Dict String ( Bool, InfoTemplate ) }
initialTemplates =
    { body =
        Dict.empty
    , info =
        Dict.empty
    }


{-| -}
orientation : Article -> Orientation
orientation s =
    case s.shape of
        Oriented o _ ->
            o

        _ ->
            Vertical


{-| -}
isBackground : Article -> Bool
isBackground s =
    case s.shape of
        Background ->
            True

        _ ->
            False


{-| -}
type Orientation
    = Vertical
    | Horizontal


{-| -}
singleton : String -> Article
singleton caption =
    { empty
        | caption = { text = caption, showsDate = False }
        , id = Layout.sanitise caption
    }


{-| -}
empty : Article
empty =
    { id = ""
    , caption = { text = "", showsDate = False }
    , info = Nothing
    , body = PeekThrough
    , shape = Oriented Vertical (Columns 1)
    , fab = Nothing
    , additionalClasses = []
    }


{-| -}
defaultIllustration : Article
defaultIllustration =
    { empty | id = "defaultIllustration", body = CustomIllustration }


{-| Title for peeks and images. May be extended later to include A11y captions.
-}
hint : ( String, Zone ) -> Article -> String
hint z s =
    s.caption.text
        ++ (if s.caption.showsDate then
                s.fab
                    |> Maybe.andThen Fab.occurrence
                    |> Maybe.map (\d -> "; on " ++ Occurrence.toString z Occurrence.Days d)
                    |> Maybe.withDefault ""

            else
                ""
           )



---- Deconstruct


{-| -}
structureClass : { c | templates : Templates } -> Article -> Html.Attribute msg
structureClass config s =
    classList [ ( "noCaption", s.caption.text == "" ), ( "hasBody", hasBody config s ) ]


{-| -}
hasBody : { c | templates : Templates } -> Article -> Bool
hasBody { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just _ ) ->
            True

        ( PeekThrough, Nothing ) ->
            False

        _ ->
            True


{-| -}
isIllustration : { c | templates : Templates } -> Article -> Bool
isIllustration { templates } s =
    case ( s.body, getTemplate .body s templates ) of
        ( _, Just (Illustration _) ) ->
            True

        ( CustomIllustration, Nothing ) ->
            True

        _ ->
            False



---- Helpers


{-| -}
orientationFromString : String -> Maybe Orientation
orientationFromString o =
    case o of
        "🀱" ->
            Just Horizontal

        "🁣" ->
            Just Vertical

        _ ->
            Nothing


{-| -}
orientationToString : Orientation -> String
orientationToString o =
    case o of
        Horizontal ->
            "🀱"

        Vertical ->
            "🁣"


{-| -}
bodyTypeToString : BodyChoice -> String
bodyTypeToString body =
    case body of
        PeekThrough ->
            "Peek-Through"

        CustomIllustration ->
            "Illustration"

        CustomContent Nothing ->
            "Html"

        CustomContent (Just n) ->
            "+" ++ n


{-| -}
infoTypeToString : Maybe InfoChoice -> String
infoTypeToString info =
    case info of
        Nothing ->
            "No byline"

        Just CustomToc ->
            "T.o.C."

        Just (CustomByline 1) ->
            "Custom line"

        Just (CustomByline n) ->
            String.fromInt n


{-| -}
toggleTemplates : Templates -> Templates
toggleTemplates t =
    { body = Dict.map (\_ ( b, x ) -> ( not b, x )) t.body
    , info = Dict.map (\_ ( b, x ) -> ( not b, x )) t.info
    }


{-| -}
templatesAreOn : Templates -> Maybe Bool
templatesAreOn =
    .body
        >> Dict.values
        >> List.head
        >> Maybe.map Tuple.first
