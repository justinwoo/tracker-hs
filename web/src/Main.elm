port module Main exposing (..)

import Html
    exposing
        ( Html
        , div
        , text
        , table
        , tbody
        , tr
        , td
        , h1
        )
import Html.App exposing (programWithFlags)
import Html.Attributes exposing (class)
import Keyboard exposing (KeyCode, downs, ups)
import List.Extra exposing ((!!))


main : Program { showData : List Show }
main =
    programWithFlags
        { init = \x -> ( { initModel | showData = x.showData }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Show =
    { name : String
    , count : String
    }


type alias Model =
    { showData : List Show
    , cursor : Int
    }


initModel : Model
initModel =
    { showData = []
    , cursor = 0
    }


type Direction
    = Up
    | Down


type Msg
    = NoOp
    | MoveCursor Direction
    | Increment
    | Decrement
    | ShowDataUpdate Show


updateShowData : Model -> Show -> Model
updateShowData model { name, count } =
    let
        mapper i x =
            if x.name == name then
                { x | count = count }
            else
                x
    in
        { model | showData = List.indexedMap mapper model.showData }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveCursor direction ->
            case direction of
                Up ->
                    ( { model | cursor = max 0 <| model.cursor - 1 }
                    , Cmd.none
                    )

                Down ->
                    ( { model | cursor = min (model.cursor + 1) <| List.length model.showData }
                    , Cmd.none
                    )

        Decrement ->
            ( model, decrementShow model )

        Increment ->
            ( model, incrementShow model )

        ShowDataUpdate x ->
            ( updateShowData model x, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type Update
    = UpdateIncrement
    | UpdateDecrement


updateShow : String -> Model -> Cmd msg
updateShow target { showData, cursor } =
    let
        name =
            case showData !! cursor of
                Just x ->
                    x.name

                Nothing ->
                    ""
    in
        updateRequests
            { target = target
            , name = name
            }


incrementShow : Model -> Cmd msg
incrementShow =
    updateShow "increment"


decrementShow : Model -> Cmd msg
decrementShow =
    updateShow "decrement"


view : Model -> Html Msg
view { showData, cursor } =
    let
        makeRow i x =
            tr
                [ class
                    <| if i == cursor then
                        "table-info"
                       else
                        ""
                ]
                [ td [] [ text x.name ]
                , td [] [ text x.count ]
                ]

        rows =
            List.indexedMap makeRow showData

        showsTable =
            div [ class "col-md-6 col-sm-12" ]
                [ table [ class "table" ]
                    [ tbody [] rows ]
                ]
    in
        div []
            [ h1 [] [ text "tracker" ]
            , showsTable
            ]


handleKeyDown : KeyCode -> Msg
handleKeyDown x =
    case x of
        74 ->
            MoveCursor Down

        75 ->
            MoveCursor Up

        _ ->
            NoOp


handleKeyUp : KeyCode -> Msg
handleKeyUp x =
    case x of
        188 ->
            Decrement

        190 ->
            Increment

        _ ->
            NoOp


port updateRequests : { target : String, name : String } -> Cmd msg


port showDataUpdates : (Show -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ downs handleKeyDown
        , ups handleKeyUp
        , showDataUpdates ShowDataUpdate
        ]
