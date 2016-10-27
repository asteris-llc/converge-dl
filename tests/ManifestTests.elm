module ManifestTests exposing (..)

import Combine
import Date exposing (Month(..))
import Date.Extra exposing (fromParts, toFormattedString)
import Expect
import Fuzz
import Manifest
import Test exposing (..)


url : Test
url =
    describe "url"
        [ test "parses a url" <|
            \() ->
                let
                    filename =
                        "s3://blah/a/b/c"
                in
                    Expect.equal
                        (Ok <| Manifest.URL "s3" "blah" [ "a", "b", "c" ])
                        (Combine.parse Manifest.url filename |> fst)
        ]


size : Test
size =
    describe "size"
        [ fuzz Fuzz.int "parses an int" <|
            \i ->
                i
                    |> toString
                    |> Combine.parse Manifest.size
                    |> fst
                    |> Expect.equal (Ok i)
        ]


date : Test
date =
    describe "date"
        [ fuzz5
            (Fuzz.intRange 1990 2016)
            (Fuzz.map (Result.withDefault Jan) <| Fuzz.map Manifest.monthFromInt <| Fuzz.intRange 1 12)
            (Fuzz.intRange 1 28)
            (Fuzz.intRange 0 23)
            (Fuzz.intRange 0 59)
            "parses a date"
          <|
            \year month day hour minute ->
                let
                    date =
                        fromParts
                            year
                            month
                            day
                            hour
                            minute
                            0
                            0
                in
                    date
                        |> toFormattedString "yyyy-MM-dd HH:mm"
                        |> Combine.parse Manifest.date
                        |> Expect.equal ( Ok date, { input = "", position = 16 } )
        ]


all : Test
all =
    concat
        [ url
        , size
        , date
        ]
