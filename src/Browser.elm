module Browser exposing (..)

import Combine
import Date.Extra as Date
import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Shorthand as Html
import Http
import List
import List.Extra as List
import Listing exposing (Listing)
import Manifest
import RemoteData exposing (RemoteData)
import Result
import String
import Style
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
        [ Style.id [ Style.Wrapper ] ]
        [ logoView
        , controlsView model
        , case model.data of
            RemoteData.NotAsked ->
                Html.div [ Style.id [ Style.Status ] ] [ Html.text "no data" ]

            RemoteData.Loading ->
                Html.div [ Style.id [ Style.Status ] ] [ Html.text "loading" ]

            RemoteData.Failure err ->
                Html.div
                    [ Style.id [ Style.Status ]
                    , Style.class [ Style.Error ]
                    ]
                    [ err |> toString |> Html.text ]

            RemoteData.Success listing ->
                case (Listing.select model.path listing) of
                    Nothing ->
                        Html.div
                            [ Style.class [ Style.Status ]
                            , Style.id [ Style.Error ]
                            ]
                            [ Html.text "Error: path not found" ]

                    Just found ->
                        listingView model found
        ]


logoView : Html Msg
logoView =
    Html.h1
        [ Style.id [ Style.Logo ] ]
        [ Html.text "Converge" ]


controlsView : Model -> Html Msg
controlsView model =
    let
        segment : List String -> Html Msg
        segment =
            \path ->
                Html.span
                    [ Style.class [ Style.NavPath ]
                    , Events.onClick <| SetPath path
                    ]
                    [ path
                        |> List.last
                        |> Maybe.withDefault "root"
                        |> Html.text
                    ]

        -- display the current path and navigate up
        segments : Html Msg
        segments =
            Html.div
                [ Style.id [ Style.NavPaths ] ]
                (model.path
                    |> List.inits
                    |> List.map segment
                    |> List.intersperse (Html.text " / ")
                )

        -- button to reload the current view
        reload : Html Msg
        reload =
            Html.button
                [ Events.onClick Reload
                , Style.id [ Style.Reload ]
                ]
                [ Html.text "Reload" ]
    in
        Html.div
            [ Style.id [ Style.Controls ] ]
            [ segments, reload ]


listingView : Model -> Listing -> Html Msg
listingView model listing =
    Html.table
        [ Style.id [ Style.Listing ] ]
        [ Html.thead_
            [ Html.tr_
                [ Html.th_ [ Html.text "name" ]
                , Html.th_ [ Html.text "size" ]
                , Html.th_ [ Html.text "date modified" ]
                ]
            ]
        , Html.tbody_ <|
            case listing of
                Listing.File _ ->
                    [ entryView (model.path) listing ]

                Listing.Directory files ->
                    files
                        |> Dict.toList
                        |> List.map (\( name, listing ) -> ( model.path ++ [ name ], listing ))
                        |> List.map (uncurry entryView)
        ]


entryView : List String -> Listing -> Html Msg
entryView path listing =
    let
        name =
            path |> List.last |> Maybe.withDefault ""
    in
        Html.tr_ <|
            case listing of
                Listing.File meta ->
                    let
                        size =
                            meta.size |> toString

                        time =
                            meta.time |> Date.toIsoString
                    in
                        [ Html.td_
                            [ Html.a
                                [ Attr.href <| String.join "/" path
                                , Attr.downloadAs name
                                ]
                                [ Html.text name ]
                            ]
                        , Html.td_ [ size |> Html.text ]
                        , Html.td_ [ time |> Html.text ]
                        ]

                Listing.Directory _ ->
                    [ Html.td
                        [ Events.onClick <| SetPath path
                        , Attr.colspan 3
                        ]
                        [ Html.text name ]
                    ]



-- UTIL


{-| up treats the input list as a path, and goes up by one level. So `up [1, 2] == [1]`
-}
up : List a -> List a
up =
    List.reverse
        >> List.tail
        >> Maybe.withDefault []
        >> List.reverse
