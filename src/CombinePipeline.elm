module CombinePipeline exposing (start, consume, ignore)

import Combine exposing (Parser)


start : a -> Parser a
start =
    Combine.succeed


consume : Parser a -> Parser (a -> res) -> Parser res
consume =
    flip Combine.andMap


ignoreOuter : Parser a -> Parser b -> Combine.Context -> ( Result (List String) b, Combine.Context )
ignoreOuter ignoreable first ctx =
    case Combine.app first ctx of
        ( Ok res, inner ) ->
            ignoreMap ignoreable res inner

        err ->
            err


ignoreMap : Parser a -> b -> Combine.Context -> ( Result (List String) b, Combine.Context )
ignoreMap ignoreable res ctx =
    case Combine.app ignoreable ctx of
        ( Ok _, ctx ) ->
            ( Ok res, ctx )

        ( Err err, ctx ) ->
            ( Err err, ctx )


ignore : Parser a -> Parser b -> Parser b
ignore ignoreable first =
    Combine.primitive (ignoreOuter ignoreable first)
