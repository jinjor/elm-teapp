module Api.Http exposing (Error, get, getIgnore, getMaybe)

import Http exposing (..)
import Json.Decode exposing (Decoder)
import Task exposing (Task)


type alias Error =
    { statusCode : Maybe Int
    , message : String
    }


get : String -> Decoder a -> Task Error a
get url decoder =
    task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver =
            resolver
                (\meta body ->
                    case Json.Decode.decodeString decoder body of
                        Ok a ->
                            Ok a

                        Err e ->
                            Err { statusCode = Just meta.statusCode, message = "Bad JSON" }
                )
        , timeout = Nothing
        }


getIgnore : String -> Task Error ()
getIgnore url =
    task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = resolver (\_ _ -> Ok ())
        , timeout = Nothing
        }


getMaybe : String -> Decoder a -> Task Error (Maybe a)
getMaybe url decoder =
    get url decoder
        |> Task.map Just
        |> Task.onError
            (\error ->
                case error.statusCode of
                    Just 404 ->
                        Task.succeed Nothing

                    _ ->
                        Task.fail error
            )


resolver : (Http.Metadata -> String -> Result Error a) -> Resolver Error a
resolver decodeBody =
    Http.stringResolver
        (\response ->
            case response of
                BadUrl_ url ->
                    Err { statusCode = Nothing, message = "Bad URL: " ++ url }

                Timeout_ ->
                    Err { statusCode = Nothing, message = "Timeout" }

                NetworkError_ ->
                    Err { statusCode = Nothing, message = "Network error" }

                BadStatus_ meta body ->
                    Err { statusCode = Just meta.statusCode, message = "Bad status" }

                GoodStatus_ meta body ->
                    decodeBody meta body
        )
