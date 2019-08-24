module Page.Login exposing (Model, Msg, init, update, view)

import Api.Endpoint
import Api.Http
import Data.Credential exposing (Credential)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LocalStorage
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
    | Response (Result Api.Http.Error (Maybe Credential))


init : Session -> ( Model, Session, Cmd Msg )
init session =
    ( { input = ""
      , authError = False
      , error = Nothing
      }
    , session
    , Cmd.none
    )


update : Msg -> Model -> Session -> ( Model, Session, Cmd Msg )
update msg model session =
    case msg of
        Input input ->
            ( { model | input = input }, session, Cmd.none )

        Submit ->
            ( { model | authError = False }
            , session
            , Api.Endpoint.login model.input
                |> Task.attempt Response
            )

        Response (Ok (Just credential)) ->
            ( model
            , session
            , LocalStorage.saveCredential (Just credential)
            )

        Response (Ok Nothing) ->
            ( { model | authError = True }
            , session
            , Cmd.none
            )

        Response (Err error) ->
            ( { model | error = Just error }
            , session
            , Cmd.none
            )


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
