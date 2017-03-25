module AddVoiceActors exposing (..)

import Html exposing (Html, table)
import Dict exposing (Dict)
import DubbedMovieSelect
import Bridge


type alias Flags =
    { titles : List String }


type alias Model =
    { ready : Bool, selects : Dict String DubbedMovieSelect.Model }


type Msg
    = Select DubbedMovieSelect.Model DubbedMovieSelect.Msg
    | Results ()
    | SelectStateChange Bridge.StateChange


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init, view = view, update = update, subscriptions = subscriptions }


model : Model
model =
    { ready = False, selects = Dict.empty }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        selects =
            (List.map (\title -> DubbedMovieSelect.init { title = title }) flags.titles)

        ( models, _ ) =
            List.unzip selects
    in
        { model
            | selects =
                (List.foldl (\model dict -> Dict.insert model.title model dict)
                    model.selects
                    models
                )
        }
            ! List.map (\( model, cmd ) -> Cmd.map (Select model) cmd) selects


selectsList : Model -> List DubbedMovieSelect.Model
selectsList model =
    Dict.values model.selects


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select selectModel msg ->
            let
                ( newSelectModel, cmd ) =
                    DubbedMovieSelect.update msg selectModel

                updatedModel =
                    { model
                        | selects =
                            (Dict.insert newSelectModel.title newSelectModel model.selects)
                    }

                newModel =
                    { updatedModel
                        | ready =
                            List.all
                                (\s -> s.state == DubbedMovieSelect.Ready)
                                (selectsList updatedModel)
                    }

                readyCmd =
                    if not model.ready && newModel.ready then
                        Bridge.ready ()
                    else
                        Cmd.none
            in
                newModel ! [ Cmd.map (Select newSelectModel) cmd, readyCmd ]

        Results () ->
            let
                entries =
                    (List.filterMap DubbedMovieSelect.adjustTitle (selectsList model))
            in
                model ! [ Bridge.selectedEntries entries ]

        SelectStateChange s ->
            case Dict.get s.title model.selects of
                Just selectModel ->
                    update (Select selectModel (DubbedMovieSelect.StateChange s)) model

                Nothing ->
                    model ! []


view : Model -> Html Msg
view model =
    table []
        (List.map (\e -> Html.map (Select e) (DubbedMovieSelect.view e)) (selectsList model))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Bridge.results Results
        , Bridge.dubbedMovieSelectState SelectStateChange
        ]
