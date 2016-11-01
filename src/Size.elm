module Size exposing (..)

import Basics


type Unit
    = Bytes Int
    | Kilobytes Float
    | Megabytes Float


rebucket : Unit -> Unit
rebucket =
    let
        unwrap : Unit -> Float
        unwrap wrapped =
            case wrapped of
                Bytes n ->
                    toFloat n

                Kilobytes n ->
                    n * 1024

                Megabytes n ->
                    n * 1024 ^ 2

        rewrap : Float -> Unit
        rewrap unwrapped =
            if unwrapped < 1024 then
                Bytes unwrapped
            else if unwrapped < 1024 ^ 2 then
                unwrapped |> (flip (/)) 1024 |> Kilobytes
            else
                unwrapped |> (flip (/)) (1024 ^ 2) |> Megabytes
    in
        unwrap >> rewrap


toString : Unit -> String
toString unit =
    let
        unitize : String -> Float -> String
        unitize unit amt =
            amt
                |> (*) 100
                |> ceiling
                |> toFloat
                |> (flip (/)) 100
                |> Basics.toString
                |> (flip (++)) unit
    in
        case unit of
            Bytes n ->
                n |> toFloat |> unitize "B"

            Kilobytes n ->
                unitize "KB" n

            Megabytes n ->
                unitize "MB" n
