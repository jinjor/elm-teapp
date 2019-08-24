module Page.Login exposing (Model, Msg, init, update, view)

import Api.Endpoint
import Api.Http
import Data.Credential exposing (Credential)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LocalStorage
import Return exposing (..)
import Session exposing (Session)
import Task


type alias Model =
    { input : String
    , authError : Bool
    , error : Maybe Api.Http.Error
    }


type Msg
    = Input String
    | Submit
    | Response (Maybe Credential)


init : Session -> Return Model Msg
init session =
    return
        { input = ""
        , authError = False
        , error = Nothing
        }


update : Msg -> Model -> Session -> Return Model Msg
update msg model session =
    case msg of
        Input input ->
            return { model | input = input }

        Submit ->
            return { model | authError = False }
                |> withRequest
                    (Api.Endpoint.login model.input
                        |> Task.map Response
                    )

        Response (Just credential) ->
            return model
                |> withCommand (LocalStorage.saveCredential (Just credential))

        Response Nothing ->
            return { model | authError = True }


view : Model -> Session -> List (Html Msg)
view model session =
    [ h1 [] [ text "Login" ]
    , Html.form [ class "login-form", onSubmit Submit ]
        [ input
            [ onInput Input
            , placeholder "GitHub user name"
            , value model.input
            ]
            []
        , button [] [ text "Login" ]
        ]
    , if model.authError then
        div [ class "error" ] [ text "Wrong user name!" ]

      else
        text ""
    ]
