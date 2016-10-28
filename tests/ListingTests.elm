module ListingTests exposing (..)

import Date exposing (Month(Jan), Date)
import Date.Extra exposing (fromParts)
import Dict exposing (Dict)
import Expect
import Fuzz
import Listing
import Test exposing (..)


baseListing : Listing.Listing
baseListing =
    Listing.File { date = fromParts 2016 Jan 1 0 0 0 0, size = 0 }


select : Test
select =
    describe "select"
        [ describe "valid paths"
            [ test "empty path" <|
                \() ->
                    baseListing
                        |> Listing.select []
                        |> Expect.equal (Just baseListing)
            , fuzz Fuzz.string "just a file" <|
                \path ->
                    baseListing
                        |> Listing.select [ path ]
                        |> Expect.equal Nothing
            , fuzz (Fuzz.list Fuzz.string) "path" <|
                \path ->
                    let
                        dir =
                            path
                                |> List.foldr
                                    (\part listing -> Listing.Directory <| Dict.singleton part listing)
                                    baseListing
                    in
                        dir
                            |> Listing.select path
                            |> Expect.equal (Just baseListing)
            ]
        ]


all : Test
all =
    concat
        [ select
        ]
