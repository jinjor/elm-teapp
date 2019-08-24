module Page.Dashboard exposing (Model, Msg, init, update, view)

import Api.Endpoint
import Api.Http
import Api.Status exposing (Status(..))
import Data.Repository exposing (Repository)
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Task


type alias Model =
    { repos : Status (List Repository)
    }


type Msg
    = GotRepos (Result Api.Http.Error (List Repository))


init : Session -> ( Model, Session, Cmd Msg )
init session =
    ( { repos = Loading
      }
    , session
    , Api.Endpoint.getRepos session.credential
        |> Task.attempt GotRepos
    )


update : Msg -> Model -> Session -> ( Model, Session, Cmd Msg )
update msg model session =
    case msg of
        GotRepos (Ok repos) ->
            ( { model | repos = Loaded repos }
            , session
            , Cmd.none
            )

        GotRepos (Err error) ->
            ( model
            , Session.setError (Just error.message) session
            , Cmd.none
            )


view : Model -> Session -> List (Html Msg)
view model session =
    [ h1 [] [ text "Repositories" ]
    , case model.repos of
        Loading ->
            text "Loading..."

        Loaded repos ->
            ul [] (List.map viewRepository repos)
    ]


viewRepository : Repository -> Html Msg
viewRepository repo =
    li [] [ text repo.name ]
