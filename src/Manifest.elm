module Manifest exposing (..)

import Combine exposing (Parser)
import Combine.Char as CChar
import Combine.Num as CNum
import CombinePipeline exposing (start, consume, ignore)
import String


type alias S3URL =
    { protocol : String
    , bucket : String
    , path : String
    }


type alias S3Size =
    Int


type Value
    = Time String
    | Size S3Size
    | Filename S3URL


whitespace : Parser Char
whitespace =
    Combine.choice
        [ CChar.space
        , CChar.tab
        , CChar.eol
        ]


s3url : Parser S3URL
s3url =
    let
        protocol =
            Combine.while ((/=) ':')

        bucket =
            Combine.while ((/=) '/')

        path =
            Combine.many CChar.anyChar |> Combine.map String.fromList
    in
        start S3URL
            |> consume protocol
            |> ignore (Combine.string "://")
            |> consume bucket
            |> consume path
