module Page.Dashboard exposing (Model, Msg, init, update, view)

import Api.Endpoint
import Api.Http
import Api.Status exposing (Status(..))
import Data.Repository exposing (Repository)
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Task
import View.Layout


type alias Model =
    { session : Session
    , repos : Status (List Repository)
    }


type Msg
    = GotRepos (Result Api.Http.Error (List Repository))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , repos = Loading
      }
    , Api.Endpoint.getRepos session.credential
        |> Task.attempt GotRepos
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRepos (Ok repos) ->
            ( { model | repos = Loaded repos }
            , Cmd.none
            )

        GotRepos (Err error) ->
            ( { model | repos = Failed error }
            , Cmd.none
            )


view : Model -> List (Html Msg)
view model =
    View.Layout.view
        { loggedIn = True
        , error =
            case model.repos of
                Failed error ->
                    Just error.message

                _ ->
                    Nothing
        , contents =
            [ h1 [] [ text "Repositories" ]
            , case model.repos of
                Loading ->
                    text "Loading..."

                Loaded repos ->
                    ul [] (List.map viewRepository repos)

                Failed error ->
                    text error.message
            ]
        }


viewRepository : Repository -> Html Msg
viewRepository repo =
    li [] [ text repo.name ]
