module Browser exposing (..)

import Combine
import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Listing exposing (Listing)
import Manifest
import RemoteData exposing (RemoteData)
import Result
import String
import Task
import Task.Extra exposing (performFailproof)


-- MODEL


type alias Model =
    { path : List String
    , data : RemoteData Err Listing
    }


type Err
    = HttpErr Http.Error
    | ParseErr (List String)


init : ( Model, Cmd Msg )
init =
    { path = [], data = RemoteData.Loading } ! [ getListing ]



-- UPDATE


type Msg
    = NewListing (RemoteData Err Listing)
    | SetPath (List String)
    | Reload


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewListing data ->
            { model | data = data } ! []

        SetPath path ->
            { model | path = path } ! []

        Reload ->
            { model | data = RemoteData.Loading } ! [ getListing ]


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



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ -- Reload button
          Html.button
            [ Events.onClick Reload
            , Attr.class "reload"
            ]
            [ Html.text "Reload" ]
          -- Segment to display the current path and navigate up
        , Html.div
            [ Events.onClick <| SetPath <| up model.path
            , Attr.class "path"
            ]
            [ model.path |> String.join "/" |> Html.text ]
          -- and finally the meat of our display: the files themselves!
        , case model.data of
            RemoteData.NotAsked ->
                Html.div [ Attr.class "status" ] [ Html.text "no data" ]

            RemoteData.Loading ->
                Html.div [ Attr.class "status" ] [ Html.text "loading" ]

            RemoteData.Failure err ->
                Html.div
                    [ Attr.class "status error" ]
                    [ err |> toString |> Html.text ]

            RemoteData.Success listing ->
                case (Listing.select model.path listing) of
                    Nothing ->
                        Html.div
                            [ Attr.class "status error" ]
                            [ Html.text "Error: path not found" ]

                    Just found ->
                        listingView model found
        ]


listingView : Model -> Listing -> Html Msg
listingView model listing =
    Html.div
        [ Attr.class "listing" ]
        [ case listing of
            Listing.File file ->
                Html.div
                    [ Attr.class "entry" ]
                    [ file |> toString |> Html.text ]

            Listing.Directory files ->
                files
                    |> Dict.toList
                    |> List.map (\( name, listing ) -> ( model.path ++ [ name ], listing ))
                    |> List.map (uncurry entryView)
                    |> Html.ul [ Attr.class "entries" ]
        ]


entryView : List String -> Listing -> Html Msg
entryView path listing =
    let
        name =
            path |> basename

        class =
            Attr.class "entry"
    in
        case listing of
            Listing.File meta ->
                Html.li
                    [ class ]
                    [ Html.a
                        [ Attr.href <| String.join "/" path
                        , Attr.downloadAs name
                        ]
                        [ name |> Html.text ]
                    , " ("
                        ++ (toString meta.size)
                        ++ " bytes, modified "
                        ++ (toString meta.time)
                        ++ ")"
                        |> Html.text
                    ]

            Listing.Directory _ ->
                Html.li
                    [ Events.onClick <| SetPath path
                    , class
                    ]
                    [ name |> Html.text ]



-- UTIL


{-| up treats the input list as a path, and goes up by one level. So `up [1, 2] == [1]`
-}
up : List a -> List a
up =
    List.reverse
        >> List.tail
        >> Maybe.withDefault []
        >> List.reverse


basename : List String -> String
basename =
    List.reverse
        >> List.head
        >> Maybe.withDefault ""
