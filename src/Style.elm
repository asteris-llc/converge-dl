module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace as Namespace


type CssIDs
    = Wrapper
    | NavPaths
    | Status


type CssClasses
    = Error
    | NavPath


primaryAccentColor =
    hex "ccffaa"


namespace =
    "converge-dl"


verticalRhythm =
    em 1.5


css =
    (stylesheet << Namespace.namespace namespace)
        [ body
            [ margin zero
            , padding zero
            , border zero
            , fontSize (pct 100)
            , fontFamily inherit
            , verticalAlign baseline
            ]
        , (#) Wrapper
            [ maxWidth (px 1820)
            , margin2 zero auto
            ]
        , (#) Status
            [ lineHeight verticalRhythm
            , border3 (px 1) solid primaryAccentColor
            , padding2 zero verticalRhythm
            , children
                [ (.) Error
                    [ border3 (px 1) solid (rgb 255 0 0) ]
                ]
            ]
        , (#) NavPaths
            [ lineHeight verticalRhythm
            , border3 (px 1) solid (rgb 0 0 0)
            , padding2 zero verticalRhythm
            , children
                [ (.) NavPath
                    [ textDecoration underline ]
                ]
            ]
        ]
