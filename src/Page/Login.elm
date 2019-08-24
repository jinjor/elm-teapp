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
import View.Layout


type alias Model =
    { session : Session
    , input : String
    , authError : Bool
    , error : Maybe Api.Http.Error
    }


type Msg
    = Input String
    | Submit
    | Response (Result Api.Http.Error (Maybe Credential))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , input = ""
      , authError = False
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( { model | input = input }, Cmd.none )

        Submit ->
            ( { model | authError = False }
            , Api.Endpoint.login model.input
                |> Task.attempt Response
            )

        Response (Ok (Just credential)) ->
            ( model
            , LocalStorage.saveCredential (Just credential)
            )

        Response (Ok Nothing) ->
            ( { model | authError = True }
            , Cmd.none
            )

        Response (Err error) ->
            ( { model | error = Just error }
            , Cmd.none
            )


view : Model -> List (Html Msg)
view model =
    View.Layout.view
        { loggedIn = False
        , error = Maybe.map .message model.error
        , contents =
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
        }
