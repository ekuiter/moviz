port module Bridge exposing (..)


type alias StateChange = { title : String, state : String }


port reportError : String -> Cmd msg


port results : (() -> msg) -> Sub msg


port ready : () -> Cmd msg


port selectedEntries : List { title : String, path : String } -> Cmd msg


port dubbedMovieSelectState : (StateChange -> msg) -> Sub msg
