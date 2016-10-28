module Main exposing (..)

import Browser
import Html.App exposing (program)


main : Program Never
main =
    program
        { init = Browser.init
        , update = Browser.update
        , view = Browser.view
        , subscriptions = \_ -> Sub.none
        }
