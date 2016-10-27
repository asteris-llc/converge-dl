port module Main exposing (..)

import Json.Encode exposing (Value)
import ManifestTests
import Test.Runner.Node exposing (run)


main : Program Value
main =
    run emit ManifestTests.all


port emit : ( String, Value ) -> Cmd msg
