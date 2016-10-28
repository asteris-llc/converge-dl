module Manifest exposing (..)

import Combine exposing (Parser)
import Combine.Char as CChar
import Combine.Infix exposing ((<*>), (<*), (*>), (<$>), (<$), (<?>))
import Combine.Num as CNum
import Date exposing (Date, Month(..))
import Date.Extra exposing (fromParts)
import String


type alias URL =
    { protocol : String
    , bucket : String
    , components : List String
    }


eol : Parser (List Char)
eol =
    Combine.many <|
        Combine.choice
            [ CChar.space
            , CChar.tab
            , CChar.eol
            , ' ' <$ Combine.end
            ]


whitespace : Parser (List Char)
whitespace =
    Combine.many <|
        Combine.choice
            [ CChar.space
            , CChar.tab
            , CChar.eol
            ]


monthFromInt : Int -> Result (List String) Month
monthFromInt month =
    case month of
        1 ->
            Ok Jan

        2 ->
            Ok Feb

        3 ->
            Ok Mar

        4 ->
            Ok Apr

        5 ->
            Ok May

        6 ->
            Ok Jun

        7 ->
            Ok Jul

        8 ->
            Ok Aug

        9 ->
            Ok Sep

        10 ->
            Ok Oct

        11 ->
            Ok Nov

        12 ->
            Ok Dec

        _ ->
            Err [ "invalid month " ++ toString month ]


month : Parser Month
month =
    let
        raw =
            nDigits 2

        outer : Combine.Context -> ( Result (List String) Month, Combine.Context )
        outer ctx =
            case Combine.app raw ctx of
                ( Ok res, after ) ->
                    ( monthFromInt res, after )

                ( Err err, after ) ->
                    ( Err err, after )
    in
        Combine.primitive outer


nDigits : Int -> Parser Int
nDigits n =
    (String.concat >> String.toInt >> Result.withDefault 0) <$> Combine.count n (Combine.regex "\\d")


date : Parser Date
date =
    fromParts
        <$> nDigits 4
        <* CChar.char '-'
        <*> month
        <* CChar.char '-'
        <*> nDigits 2
        <* CChar.space
        <*> nDigits 2
        <* CChar.char ':'
        <*> nDigits 2
        <*> Combine.succeed 0
        <*> Combine.succeed 0


size : Parser Int
size =
    CNum.int


url : Parser URL
url =
    let
        protocol =
            Combine.while ((/=) ':')

        component =
            Combine.while ((/=) '/')

        slash =
            CChar.char '/'
    in
        URL
            <$> protocol
            <* (Combine.string "://")
            <*> component
            <* slash
            <*> Combine.sepBy1 slash component
