module Listing exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import List
import Maybe


type Listing
    = Directory (Dict String Listing)
    | File { date : Date, size : Int }


select : List String -> Listing -> Maybe Listing
select path listing =
    case path of
        [] ->
            Just listing

        part :: parts ->
            case listing of
                File _ ->
                    Nothing

                Directory members ->
                    members
                        |> Dict.get part
                        |> (flip Maybe.andThen) (select parts)
