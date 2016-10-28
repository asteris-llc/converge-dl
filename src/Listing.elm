module Listing exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import List
import Manifest
import Maybe


type Listing
    = Directory (Dict String Listing)
    | File { time : Date, size : Int }


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


fromManifest : List Manifest.Line -> Listing
fromManifest manifest =
    let
        file : Manifest.Line -> Listing
        file line =
            File
                { time = line.time
                , size = line.size
                }

        path : Manifest.Line -> List String
        path =
            .url >> .components

        insertAt : Listing -> List String -> Listing -> Listing
        insertAt listing path dest =
            case dest of
                File _ ->
                    dest

                Directory dir ->
                    case path of
                        [] ->
                            dest

                        part :: [] ->
                            dir
                                |> Dict.insert part listing
                                |> Directory

                        part :: parts ->
                            dir
                                |> Dict.insert
                                    part
                                    (insertAt
                                        listing
                                        parts
                                        (Dict.get part dir |> Maybe.withDefault (Directory <| Dict.empty))
                                    )
                                |> Directory

        combine : Manifest.Line -> Listing -> Listing
        combine line dir =
            insertAt (file line) (path line) dir
    in
        List.foldl combine (Directory <| Dict.empty) manifest
