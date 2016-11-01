port module Main exposing (..)

import Json.Encode exposing (Value)
import ListingTests
import ManifestTests
import SizeTests
import Test exposing (concat)
import Test.Runner.Node exposing (run)


main : Program Value
main =
    run emit <|
        concat
            [ ManifestTests.all
            , ListingTests.all
            , SizeTests.all
            ]


port emit : ( String, Value ) -> Cmd msg
