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
        , describe "megabytes"
            [ fuzz (Fuzz.intRange 1024 (1024 ^ 2 - 1)) "from bytes" <|
                \i ->
                    i
                        |> Size.Bytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> (flip (/)) 1024 |> Size.Megabytes)
            , fuzz (Fuzz.intRange 1 1023) "from megabytes" <|
                \i ->
                    i
                        |> toFloat
                        |> Size.Megabytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> Size.Megabytes)
            ]
        , describe "gigabytes"
            [ fuzz (Fuzz.intRange (1024 ^ 2) (1024 ^ 3 - 1)) "from bytes" <|
                \i ->
                    i
                        |> Size.Bytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> (flip (/)) (1024 ^ 2) |> Size.Gigabytes)
            , fuzz (Fuzz.intRange 1 1023) "from gigabytes" <|
                \i ->
                    i
                        |> toFloat
                        |> Size.Gigabytes
                        |> Size.rebucket
                        |> Expect.equal (i |> toFloat |> Size.Gigabytes)
            ]
        ]


all : Test
all =
    concat [ rebucket ]
