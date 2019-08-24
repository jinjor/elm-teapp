module Api.Endpoint exposing (getRepos, login)

import Api.Http exposing (Error)
import Data.Credential exposing (Credential)
import Data.Repository exposing (Repository)
import Json.Decode exposing (list)
import Task exposing (Task)
import Url.Builder


login : String -> Task Error (Maybe Credential)
login userName =
    Api.Http.getIgnore
        (Url.Builder.crossOrigin "https://api.github.com"
            [ "users", userName ]
            []
        )
        |> Task.map
            (\_ ->
                Just
                    { userName = userName
                    , token = "CC45F396-F5F7-472B-9BB9-D0B4C9EEC0E1"
                    }
            )
        |> Task.onError
            (\error ->
                case error.statusCode of
                    Just 404 ->
                        Task.succeed Nothing

                    _ ->
                        Task.fail error
            )


getRepos : Maybe Credential -> Task Error (List Repository)
getRepos credential =
    case credential of
        Just { userName } ->
            Api.Http.get
                (Url.Builder.crossOrigin "https://api.github.com"
                    [ "users", userName, "repos" ]
                    []
                )
                (list Data.Repository.decoder)

        Nothing ->
            Task.fail { statusCode = Nothing, message = "No token" }
