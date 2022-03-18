module Layout exposing (..)

import Css exposing (..)
import Css.Global as Global
import Css.Media as Media
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)



---- Theming ----


theme =
    { f0 = rgba 255 255 255 0.9
    , b0 = rgb 0 0 0
    , f1 = rgb 255 255 255
    , b1 = rgba 255 255 255 0.1
    , fp = rgb 255 255 255
    , bp = rgb 80 80 255
    , fa = rgb 255 255 255
    , fb = rgb 0 255 0
    , li = rgb 140 140 255
    , green = rgb 80 230 120
    , raised = rgba 255 255 255 0.2
    , blue = rgb 0 0 255
    }


typography =
    Global.global
        [ Global.selector "html"
            [ color theme.f0
            , backgroundColor theme.b0
            , fontFamilies [ "subarubook", "sans" ]
            , lineHeight rhythm.line
            , Media.withMediaQuery [ "screen and (max-width: 335px)" ]
                [ fontSize (vw (100 / 24)) ]
            , Media.withMediaQuery [ "screen and (max-width: 359px)" ]
                [ fontSize (rem (14 / 16)) ]
            , Media.withMediaQuery [ "screen and (max-width: 383px)" ]
                [ fontSize (rem (15 / 16)) ]
            ]
        , Global.selector "body"
            [ Media.withMediaQuery [ "screen and (max-width: 335px)" ]
                [ fontSize (px 14) ]
            ]
        , Global.selector "div, p, a, section, article, header, footer, main"
            [ boxSizing borderBox ]
        , Global.selector "b, strong"
            [ fontFamilies [ "subarumedium", "sans" ] ]
        , Global.selector "i, em"
            [ fontFamilies [ "subarubook_italic", "sans" ] ]
        , Global.selector "h1"
            [ fontFamilies [ "subarumedium", "sans" ]
            , fontSize (rem 1)
            ]
        , Global.selector "p" pStyle
        , Global.selector "h2" h2Style
        , Global.selector ".E" expandedStyle
        , Global.selector ".F" focusedStyle
        ]


expandedStyle =
    [ backgroundColor theme.raised ]


focusedStyle =
    [ backgroundColor theme.blue ]


unit fu =
    rem (fu 3)


rhythm =
    { default = em 1
    , line = em 1.5
    , denseLine = em 1
    , columnWidth = rem (7 * 3)
    , break = rem 1.5
    , padding = rem (9 / 8)
    , verticalPadding = rem 1
    , minMargin = rem 1.5
    }



---- Elements ----


p =
    Html.text
        >> List.singleton
        >> Html.p [ css pStyle ]


pStyle =
    [ maxWidth rhythm.columnWidth
    , margin4 zero zero rhythm.break zero
    , padding2 zero rhythm.padding
    ]


anchoredLabel t =
    let
        sanitisedString =
            String.replace " " "-" t
    in
    Html.text t
        |> List.singleton
        >> Html.h1
            [ css
                [ fontFamilies [ "subaruheavy", "sans" ]
                , fontSize rhythm.default
                , lineHeight rhythm.default
                , margin zero
                ]
            , Attributes.id sanitisedString
            ]
        |> List.singleton
        >> Html.a
            [ Attributes.href ("#" ++ sanitisedString)
            , css
                [ link
                    [ textDecoration inherit
                    , color theme.li
                    ]
                , visited
                    [ color theme.li
                    ]
                , outline3 (px 1) dashed theme.li
                , display block
                , padding2 rhythm.verticalPadding rhythm.padding
                , maxWidth rhythm.columnWidth
                ]
            ]


h2Style =
    [ maxWidth rhythm.columnWidth
    , fontFamilies [ "subarumedium", "sans" ]
    , padding2 zero rhythm.padding
    , fontSize rhythm.default
    , color theme.green
    , margin4 rhythm.break zero rhythm.break zero
    ]


h2 t =
    let
        sanitisedString =
            String.replace " " "-" t
    in
    Html.text t
        |> List.singleton
        >> Html.h2
            [ css h2Style
            , Attributes.id sanitisedString
            ]
        |> List.singleton
        >> Html.a
            [ Attributes.href ("#" ++ sanitisedString)
            , css [ color inherit, hover [ textDecoration underline ] ]
            ]


dense =
    Html.text
        >> List.singleton
        >> Html.p
            [ css
                [ maxWidth rhythm.columnWidth
                , fontFamilies [ "subarumedium", "sans" ]
                , lineHeight rhythm.denseLine
                , padding2 zero rhythm.padding
                , margin4 zero zero rhythm.break zero
                ]
            ]


header : String -> String -> String -> Html msg
header query id t =
    Html.text t
        |> List.singleton
        >> Html.h1
            [ css
                [ fontFamilies [ "subaruheavy", "sans" ]
                , fontSize rhythm.default
                , lineHeight rhythm.default
                , margin zero
                ]
            , Attributes.id (id ++ "|label")
            ]
        |> List.singleton
        >> Html.a
            [ Attributes.href
                ((if query == "" then
                    ""

                  else
                    "?" ++ query
                 )
                    ++ "#"
                    ++ id
                )
            , css
                [ link
                    [ textDecoration inherit
                    , color theme.li
                    ]
                , visited
                    [ color theme.li
                    ]
                , outline3 (px 1) dashed theme.li
                , display block
                , padding2 rhythm.verticalPadding rhythm.padding
                , maxWidth rhythm.columnWidth
                ]
            ]


section =
    Html.section [ css [ margin2 zero rhythm.minMargin ] ]
