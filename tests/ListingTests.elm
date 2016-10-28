module ListingTests exposing (..)

import Date exposing (Month(Jan), Date)
import Date.Extra exposing (fromParts)
import Dict exposing (Dict)
import Expect
import Fuzz
import Listing
import Manifest
import Test exposing (..)


baseTime : Date
baseTime =
    fromParts 2016 Jan 1 0 0 0 0


baseSize : Int
baseSize =
    0


baseListing : Listing.Listing
baseListing =
    Listing.File { time = baseTime, size = baseSize }


listingAtPath : Listing.Listing -> List String -> Listing.Listing
listingAtPath listing path =
    path
        |> List.foldr
            (\part listing -> Listing.Directory <| Dict.singleton part listing)
            listing


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
                    path
                        |> listingAtPath baseListing
                        |> Listing.select path
                        |> Expect.equal (Just baseListing)
            ]
        ]


fromManifest : Test
fromManifest =
    describe "fromManifest"
        [ test "converts a line" <|
            \() ->
                let
                    path =
                        [ "a" ]

                    line =
                        Manifest.Line baseTime baseSize { protocol = "s3", bucket = "", components = path }

                    listing =
                        listingAtPath baseListing path
                in
                    [ line ]
                        |> Listing.fromManifest
                        |> Expect.equal listing
        , test "converts shallow" <|
            \() ->
                let
                    lines =
                        [ Manifest.Line
                            baseTime
                            baseSize
                            { protocol = "s3", bucket = "", components = [ "a" ] }
                        , Manifest.Line
                            baseTime
                            baseSize
                            { protocol = "s3", bucket = "", components = [ "b" ] }
                        ]

                    listing =
                        Listing.Directory <|
                            Dict.fromList
                                [ ( "a", baseListing )
                                , ( "b", baseListing )
                                ]
                in
                    lines
                        |> Listing.fromManifest
                        |> Expect.equal listing
        , test "converts deep" <|
            \() ->
                let
                    lines =
                        [ Manifest.Line baseTime baseSize { protocol = "s3", bucket = "", components = [ "a", "b" ] } ]

                    listing =
                        Listing.Directory <|
                            Dict.fromList
                                [ ( "a", Listing.Directory <| Dict.fromList [ ( "b", baseListing ) ] )
                                ]
                in
                    lines
                        |> Listing.fromManifest
                        |> Expect.equal listing
        ]


all : Test
all =
    concat
        [ select
        , fromManifest
        ]
