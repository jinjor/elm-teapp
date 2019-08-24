module Api.Status exposing (Status(..))


type Status a
    = Loading
    | Loaded a
