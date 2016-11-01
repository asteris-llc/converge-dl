module SizeTests exposing (..)

import Expect
import Fuzz
import Size
import Test exposing (..)


rebucket : Test
rebucket =
    describe "toUnit"
        [ describe "bytes"
            [ fuzz (Fuzz.intRange 0 1023) "bytes" <|
                \i ->
                    i
                        |> Size.Bytes
                        |> Size.rebucket
                        |> Expect.equal (Size.Bytes i)
            ]
        , describe "kilobytes"
            [ fuzz (Fuzz.intRange 1024 (1024 ^ 2 - 1)) "from bytes" <|
                \i ->
                    i
                        |> Size.Bytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> (flip (/)) 1024 |> Size.Kilobytes)
            , fuzz (Fuzz.intRange 1 1023) "from kilobytes" <|
                \i ->
                    i
                        |> toFloat
                        |> Size.Kilobytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> Size.Kilobytes)
            ]
        , describe "megabytes"
            [ fuzz (Fuzz.intRange (1024 ^ 2) (1024 ^ 3 - 1)) "from bytes" <|
                \i ->
                    i
                        |> Size.Bytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> (flip (/)) (1024 ^ 2) |> Size.Megabytes)
            , fuzz (Fuzz.intRange 1 1023) "from megabytes" <|
                \i ->
                    i
                        |> toFloat
                        |> Size.Megabytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> Size.Megabytes)
            ]
        ]


all : Test
all =
    concat [ rebucket ]
