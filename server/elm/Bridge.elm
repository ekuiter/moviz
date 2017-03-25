port module Bridge exposing (..)


port reportError : String -> Cmd msg


port results : (() -> msg) -> Sub msg


port selectedEntries : List { title : String, path : String } -> Cmd msg
