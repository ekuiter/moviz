module DubbedMovieSelect exposing (..)

import Html exposing (Html, tr, td, label, select, text, option, div)
import Html.Attributes exposing (for, id, class)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (field, string, list, oneOf, null)
import Http
import Bridge


type alias Flags =
    { title : String }


type alias DubbedMovie =
    { title : String, path : String }


type DubbedMovieEntry
    = Entry DubbedMovie
    | SelectionEmpty


dubbedMovieDecoder : Decode.Decoder DubbedMovie
dubbedMovieDecoder =
    Decode.map2 DubbedMovie (field "title" string) (field "path" string)


type State
    = Loading
    | Ready
    | Done


type alias Model =
    { state : State
    , title : String
    , entries : List DubbedMovieEntry
    , selectedEntry : Maybe DubbedMovie
    }


type Msg
    = Change String
    | NewSuggestions (Result Http.Error (List DubbedMovie))
    | StateChange Bridge.StateChange


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init, view = view, update = update, subscriptions = subscriptions }


model : Model
model =
    { state = Loading, title = "", entries = [], selectedEntry = Nothing }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { model | title = flags.title } ! [ getSuggestions flags.title ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change title ->
            let
                dubbedMovie =
                    if isActualTitle title then
                        (List.head (List.filterMap (isSelectedEntry title) model.entries))
                    else
                        Nothing
            in
                { model | selectedEntry = dubbedMovie } ! []

        NewSuggestions (Ok suggestions) ->
            { model
                | state = Ready
                , entries = SelectionEmpty :: (List.map Entry suggestions)
                , selectedEntry = Nothing
            }
                ! []

        NewSuggestions (Err err) ->
            model ! [ Bridge.reportError (toString err) ]

        StateChange s ->
            { model | state = stringToState s.state, selectedEntry = Nothing } ! []


view : Model -> Html Msg
view model =
    let
        elem =
            case model.state of
                Loading ->
                    div [ class "progress" ] []

                Ready ->
                    select [ id model.title, onChange Change ] (List.map makeOption model.entries)

                Done ->
                    text "Done"
    in
        tr []
            [ td [] [ label [ for model.title ] [ text model.title ] ]
            , td [] [ elem ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Bridge.dubbedMovieSelectState StateChange


stringToState : String -> State
stringToState str =
    case str of
        "loading" ->
            Loading

        "ready" ->
            Ready

        "done" ->
            Done

        _ ->
            Loading


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Decode.map handler <| Decode.at [ "target", "value" ] string


getSuggestions : String -> Cmd Msg
getSuggestions title =
    let
        request =
            Http.get ("/synchronkartei/suggest/" ++ title)
                (oneOf [ null [], (list dubbedMovieDecoder) ])
    in
        Http.send NewSuggestions request


isSelectedEntry : String -> DubbedMovieEntry -> Maybe DubbedMovie
isSelectedEntry title entry =
    case entry of
        Entry dubbedMovie ->
            if dubbedMovie.title == title then
                Just dubbedMovie
            else
                Nothing

        SelectionEmpty ->
            Nothing


loadingTitle : String
loadingTitle =
    "Loading ..."


selectionEmptyTitle : String
selectionEmptyTitle =
    "-"


makeOption : DubbedMovieEntry -> Html Msg
makeOption entry =
    let
        title =
            case entry of
                Entry dubbedMovie ->
                    dubbedMovie.title

                SelectionEmpty ->
                    selectionEmptyTitle
    in
        option [] [ text title ]


isActualTitle : String -> Bool
isActualTitle title =
    title /= loadingTitle && title /= selectionEmptyTitle


adjustTitle : Model -> Maybe DubbedMovie
adjustTitle model =
    Maybe.map (\entry -> { entry | title = model.title }) model.selectedEntry
