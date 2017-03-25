module DubbedMovieSelect exposing (..)

import Html exposing (Html, div, select, option, text)
import Html.Events exposing (onInput)
import Json.Decode as Decode exposing (field, string, list, oneOf, null)
import Http
import Bridge


type alias Flags =
    { title : String }


type alias DubbedMovie =
    { title : String, path : String }


type DubbedMovieEntry
    = Entry DubbedMovie
    | Loading
    | SelectionEmpty


dubbedMovieDecoder : Decode.Decoder DubbedMovie
dubbedMovieDecoder =
    Decode.map2 DubbedMovie (field "title" string) (field "path" string)


type alias Model =
    { title : String, entries : List DubbedMovieEntry, selectedEntry : Maybe DubbedMovie }


type Msg
    = Change String
    | NewSuggestions (Result Http.Error (List DubbedMovie))


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init, view = view, update = update, subscriptions = always Sub.none }


model : Model
model =
    { title = "", entries = [ Loading ], selectedEntry = Nothing }


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
                | entries = SelectionEmpty :: (List.map Entry suggestions)
                , selectedEntry = Nothing
            }
                ! []

        NewSuggestions (Err err) ->
            model ! [ Bridge.reportError (toString err) ]


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput Change ] (List.map makeOption model.entries)
        , text (optionTitle model.selectedEntry)
        ]


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

        Loading ->
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

                Loading ->
                    loadingTitle

                SelectionEmpty ->
                    selectionEmptyTitle
    in
        option [] [ text title ]


isActualTitle : String -> Bool
isActualTitle title =
    title /= loadingTitle && title /= selectionEmptyTitle


optionTitle : Maybe DubbedMovie -> String
optionTitle entry =
    Maybe.withDefault "(Nothing)" (Maybe.map (\e -> e.title ++ " (" ++ e.path ++ ")") entry)


adjustTitle : Model -> Maybe DubbedMovie
adjustTitle model =
    Maybe.map (\entry -> { entry | title = model.title }) model.selectedEntry
