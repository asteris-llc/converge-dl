module Browser exposing (..)

import Combine
import Html exposing (Html)
import Http
import Listing exposing (Listing)
import Manifest
import RemoteData exposing (RemoteData)
import Result
import Task
import Task.Extra exposing (performFailproof)


type alias Model =
    RemoteData Err Listing


init : ( Model, Cmd Msg )
init =
    RemoteData.NotAsked ! [ getListing ]


type Err
    = HttpErr Http.Error
    | ParseErr (List String)


type Msg
    = NewListing (RemoteData Err Listing)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewListing data ->
            data ! []


getListing : Cmd Msg
getListing =
    let
        parse : Result Err String -> Result Err (List Manifest.Line)
        parse =
            (flip Result.andThen)
                (Combine.parse Manifest.lines
                    >> fst
                    >> Result.formatError ParseErr
                )
    in
        Http.getString "/manifest.txt"
            |> Task.mapError HttpErr
            |> Task.toResult
            |> Task.map parse
            |> Task.map (Result.map Listing.fromManifest)
            |> performFailproof (RemoteData.fromResult >> NewListing)


view : Model -> Html Msg
view model =
    model |> toString |> Html.text
