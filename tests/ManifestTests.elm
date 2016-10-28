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
                "s3://converge-builds-dl/0.3.0-1-g9379a1d/converge_0.3.0-1-g9379a1d_darwin_386/converge.sha256sum"
                    |> Combine.parse Manifest.url
                    |> Expect.equal
                        ( Ok <| Manifest.URL "s3" "converge-builds-dl" [ "0.3.0-1-g9379a1d", "converge_0.3.0-1-g9379a1d_darwin_386", "converge.sha256sum" ]
                        , { input = "", position = 96 }
                        )
        , test "leaves trailing chars" <|
            \() ->
                "s3://converge-builds-dl/0.3.0-1-g9379a1d/converge_0.3.0-1-g9379a1d_darwin_386/converge.sha256sum extra"
                    |> Combine.parse Manifest.url
                    |> Expect.equal
                        ( Ok <| Manifest.URL "s3" "converge-builds-dl" [ "0.3.0-1-g9379a1d", "converge_0.3.0-1-g9379a1d_darwin_386", "converge.sha256sum" ]
                        , { input = " extra", position = 96 }
                        )
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
                        |> Expect.equal
                            ( Ok date
                            , { input = "", position = 16 }
                            )
        ]


line : Test
line =
    describe "line"
        [ test "parses a line" <|
            \() ->
                "2016-10-27 16:09  10492296   s3://converge-builds-dl/0.3.0-1-g9379a1d/converge_0.3.0-1-g9379a1d_darwin_386/converge"
                    |> Combine.parse Manifest.line
                    |> Expect.equal
                        ( Ok
                            { time = fromParts 2016 Oct 27 16 9 0 0
                            , size = 10492296
                            , url = Manifest.URL "s3" "converge-builds-dl" [ "0.3.0-1-g9379a1d", "converge_0.3.0-1-g9379a1d_darwin_386", "converge" ]
                            }
                        , { input = "", position = 115 }
                        )
        ]


sampleLines : String
sampleLines =
    """2016-10-27 16:09  10492296   s3://converge-builds-dl/0.3.0-1-g9379a1d/converge_0.3.0-1-g9379a1d_darwin_386/converge
2016-10-27 16:09        77   s3://converge-builds-dl/0.3.0-1-g9379a1d/converge_0.3.0-1-g9379a1d_darwin_386/converge.sha256sum
"""


lines : Test
lines =
    describe "lines"
        [ test "parses a line" <|
            \() ->
                "2016-10-27 16:09  10492296   s3://converge-builds-dl/0.3.0-1-g9379a1d/converge_0.3.0-1-g9379a1d_darwin_386/converge"
                    |> Combine.parse Manifest.lines
                    |> Expect.equal
                        ( Ok
                            [ { time = fromParts 2016 Oct 27 16 9 0 0
                              , size = 10492296
                              , url = Manifest.URL "s3" "converge-builds-dl" [ "0.3.0-1-g9379a1d", "converge_0.3.0-1-g9379a1d_darwin_386", "converge" ]
                              }
                            ]
                        , { input = "", position = 115 }
                        )
        , test "parses lines" <|
            \() ->
                sampleLines
                    |> Combine.parse Manifest.lines
                    |> Expect.equal
                        ( Ok
                            [ { time = fromParts 2016 Oct 27 16 9 0 0
                              , size = 10492296
                              , url = Manifest.URL "s3" "converge-builds-dl" [ "0.3.0-1-g9379a1d", "converge_0.3.0-1-g9379a1d_darwin_386", "converge" ]
                              }
                            , { time = fromParts 2016 Oct 27 16 9 0 0
                              , size = 77
                              , url = Manifest.URL "s3" "converge-builds-dl" [ "0.3.0-1-g9379a1d", "converge_0.3.0-1-g9379a1d_darwin_386", "converge.sha256sum" ]
                              }
                            ]
                        , { input = "", position = 242 }
                        )
        ]


all : Test
all =
    concat
        [ url
        , size
        , date
        , line
        , lines
        ]
