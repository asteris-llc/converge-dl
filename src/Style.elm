module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, thead, tbody, td, tr, th)
import Css.Namespace as Namespace
import Html.CssHelpers
import String


type CssIDs
    = Wrapper
    | NavPaths
    | Status
    | Logo
    | Controls
    | Reload
    | Listing


type CssClasses
    = Error
    | NavPath


namespace : String
namespace =
    "converge-dl"


{ id, class, classList } =
    Html.CssHelpers.withNamespace namespace



-- THEME


type alias Theme =
    { base : Color
    , text : Color
    , box :
        { background : Color
        }
    , listing :
        { divider : Color
        , header : Color
        }
    }


baseFontSize : Px
baseFontSize =
    px 18


baseBorderRadius : Px
baseBorderRadius =
    px 4


verticalRhythm : Px
verticalRhythm =
    px 25


theme : Theme
theme =
    let
        gray =
            \level -> rgb level level level

        white =
            gray 255

        blackish =
            gray 74

        lightish =
            gray 129
    in
        { base = gray 248
        , text = blackish
        , box =
            { background = white
            }
        , listing =
            { divider = gray 221
            , header = lightish
            }
        }



-- STYLES


style : Theme -> List Snippet
style theme =
    let
        box =
            mixin
                [ borderRadius baseBorderRadius
                , backgroundColor theme.box.background
                , property "box-shadow" "1px 1px 2px 0px rgba(0,0,0,0.50)"
                , border zero
                ]

        fixMath : { a | value : String } -> { a | value : String }
        fixMath v =
            { v | value = v.value |> String.filter ((/=) ' ') }
    in
        [ body
            [ fontSize baseFontSize
            , backgroundColor theme.base
            , color theme.text
            ]
        , (#) Wrapper
            [ maxWidth (px 1024)
            , margin2 zero auto
            ]
        , (#) Logo
            [ fontSize baseFontSize
            , fontWeight (int 500)
            ]
        , (#) Controls
            [ displayFlex
            , flexDirection row
            , property "justify-content" "space-between"
            , alignItems center
            , height (px 50)
            , children
                [ (#) NavPaths
                    [ children
                        [ (.) NavPath [ textDecoration underline ] ]
                    ]
                , (#) Reload
                    [ padding2 zero (verticalRhythm |/| (px 2) |> fixMath)
                    , lineHeight (verticalRhythm |*| (px 1.7) |> fixMath)
                    , box
                    ]
                ]
            ]
        , (#) Listing
            [ box
            , width (pct 100)
            , marginTop (verticalRhythm |/| (px 2) |> fixMath)
            , children
                [ thead
                    [ descendants
                        [ th
                            [ textAlign left
                            , color theme.listing.header
                            , fontWeight (int 500)
                            , textTransform capitalize
                            ]
                        ]
                    ]
                ]
            , descendants
                [ tr
                    [ lineHeight (verticalRhythm |*| (px 2) |> fixMath)
                    , children <|
                        List.map
                            (\el ->
                                el
                                    [ borderBottom3 (px 1) solid theme.listing.divider
                                    , property "border-collapse" "collapse"
                                    , firstChild [ paddingLeft verticalRhythm ]
                                    , lastChild [ paddingRight verticalRhythm ]
                                    ]
                            )
                            [ td, th ]
                    , lastChild [ children [ td [ border zero ] ] ]
                    ]
                ]
            ]
        ]


css : Stylesheet
css =
    style theme
        |> Namespace.namespace namespace
        |> stylesheet
