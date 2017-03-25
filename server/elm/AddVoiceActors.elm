module AddVoiceActors exposing (..)

import Html exposing (Html, div)
import Dict exposing (Dict)
import DubbedMovieSelect
import Bridge


type alias Flags =
    { titles : List String }


type alias Model =
    Dict String DubbedMovieSelect.Model


type Msg
    = Select DubbedMovieSelect.Model DubbedMovieSelect.Msg
    | Results ()


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init, view = view, update = update, subscriptions = subscriptions }


model : Model
model =
    Dict.empty


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        selects =
            (List.map (\title -> DubbedMovieSelect.init { title = title }) flags.titles)

        ( models, _ ) =
            List.unzip selects
    in
        (List.foldl (\model dict -> Dict.insert model.title model dict) model models)
            ! List.map (\( model, cmd ) -> Cmd.map (Select model) cmd) selects


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select selectModel msg ->
            let
                ( newSelectModel, cmd ) =
                    DubbedMovieSelect.update msg selectModel
            in
                (Dict.insert newSelectModel.title newSelectModel model)
                    ! [ Cmd.map (Select newSelectModel) cmd ]

        Results () ->
            let
                entries =
                    (List.filterMap DubbedMovieSelect.adjustTitle (Dict.values model))
            in
                model ! [ Bridge.selectedEntries entries ]


view : Model -> Html Msg
view model =
    div []
        (List.map (\e -> Html.map (Select e) (DubbedMovieSelect.view e)) (Dict.values model))


subscriptions : Model -> Sub Msg
subscriptions model =
    Bridge.results Results
