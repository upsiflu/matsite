module Layout exposing (..)

import Css exposing (..)
import Css.Global as Global
import Css.Media as Media
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class, css, href)
import Svg.Styled as Svg exposing (svg)
import Svg.Styled.Attributes as SvgAttributes



---- Variables ----


toProperty ( key, value ) =
    Css.property ("--" ++ key) (String.fromInt value)



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
    , li2 = rgb 80 80 255
    , green = rgb 80 230 120
    , raised = rgba 255 255 255 0.2
    , blue = rgb 0 0 255
    }


typography : Html msg
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
        , Global.selector "b, strong, a"
            [ fontFamilies [ "subarumedium", "sans" ] ]
        , Global.selector "i, em"
            [ fontFamilies [ "subarubook_italic", "sans" ] ]
        , Global.selector "h1"
            [ fontFamilies [ "subarumedium", "sans" ]
            , fontSize (rem 1)
            ]
        , Global.selector "p" pStyle
        , Global.selector "a" aStyle
        , Global.selector "h2" h2Style
        , Global.selector ".bleeding" bleedingStyle
        , Global.selector "img" bleedingStyle
        ]


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


hamburgerMenu : String -> Html msg
hamburgerMenu destination =
    Html.a [ class "hamburgerMenu", href destination ]
        [ svg [ SvgAttributes.viewBox "0 0 32 32", SvgAttributes.width "100%" ]
            [ Svg.path [ SvgAttributes.d "M4,10h24c1.104,0,2-0.896,2-2s-0.896-2-2-2H4C2.896,6,2,6.896,2,8S2.896,10,4,10z M28,14H4c-1.104,0-2,0.896-2,2  s0.896,2,2,2h24c1.104,0,2-0.896,2-2S29.104,14,28,14z M28,22H4c-1.104,0-2,0.896-2,2s0.896,2,2,2h24c1.104,0,2-0.896,2-2  S29.104,22,28,22z" ] [] ]
        ]


p =
    Html.text
        >> List.singleton
        >> Html.p [ css pStyle ]


pStyle =
    [ maxWidth rhythm.columnWidth
    , margin4 zero zero rhythm.break zero
    , padding2 zero rhythm.padding
    ]


bleedingStyle =
    [ maxWidth (pct 100)
    ]


aStyle =
    [ link
        [ textDecoration inherit
        , color theme.li
        ]
    , visited
        [ color theme.li2
        ]
    ]


sanitise : String -> String
sanitise =
    String.replace " " "-"
        >> String.replace "/" "-"


anchoredLabel t =
    Html.text t
        |> List.singleton
        >> Html.h1
            [ css
                [ fontFamilies [ "subaruheavy", "sans" ]
                , fontSize rhythm.default
                , lineHeight rhythm.default
                , margin zero
                ]
            , Attributes.id (sanitise t)
            ]
        |> List.singleton
        >> Html.a
            [ Attributes.href (sanitise t)
            , css
                [ link
                    [ textDecoration inherit
                    , color theme.li
                    ]
                , visited
                    [ color theme.li2
                    ]

                --, outline3 (px 1) dashed theme.li
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
            [ Attributes.href (" " ++ sanitisedString)
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
                (sanitise id
                    ++ (if query == "" then
                            ""

                        else
                            "?" ++ query
                       )
                )
            , Attributes.class "segmentLabel"
            , css
                [ link
                    [ textDecoration inherit
                    , color theme.li
                    ]
                , visited
                    [ color theme.li
                    ]

                --, outline3 (px 1) dashed theme.li
                , display block
                , padding2 rhythm.verticalPadding rhythm.padding
                ]
            ]


section =
    Html.section [ css [ margin2 zero rhythm.minMargin ] ]


byline =
    Html.text
