module Fuzzers exposing (date, manifestURL, manifestLine)

import Date exposing (Date, Month(Jan))
import Date.Extra exposing (fromParts)
import Fuzz exposing (..)
import Manifest


andMap_ =
    flip andMap


date : Fuzzer Date
date =
    constant fromParts
        |> andMap_ (intRange 1990 2016)
        |> andMap_ (Fuzz.map (Result.withDefault Jan) <| Fuzz.map Manifest.monthFromInt <| Fuzz.intRange 1 12)
        |> andMap_ (intRange 1 28)
        |> andMap_ (intRange 0 23)
        |> andMap_ (intRange 0 59)
        |> andMap_ (constant 0)
        |> andMap_ (constant 0)


manifestURL : Fuzzer Manifest.URL
manifestURL =
    constant Manifest.URL
        |> andMap_ string
        |> andMap_ string
        |> andMap_ (list string)


manifestLine : Fuzzer Manifest.Line
manifestLine =
    constant Manifest.Line
        |> andMap_ date
        |> andMap_ int
        |> andMap_ manifestURL
