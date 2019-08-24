module Api.Status exposing (Status(..))

import Api.Http


type Status a
    = Loading
    | Loaded a
    | Failed Api.Http.Error
