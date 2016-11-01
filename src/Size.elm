module Size exposing (..)


type Unit
    = Bytes Int
    | Megabytes Float
    | Gigabytes Float


rebucket : Unit -> Unit
rebucket =
    let
        unwrap : Unit -> Float
        unwrap wrapped =
            case wrapped of
                Bytes n ->
                    toFloat n

                Megabytes n ->
                    n * 1024

                Gigabytes n ->
                    n * 1024 ^ 2

        rewrap : Float -> Unit
        rewrap unwrapped =
            if unwrapped < 1024 then
                Bytes unwrapped
            else if unwrapped < 1024 ^ 2 then
                unwrapped |> (flip (/)) 1024 |> Megabytes
            else
                unwrapped |> (flip (/)) (1024 ^ 2) |> Gigabytes
    in
        unwrap >> rewrap
