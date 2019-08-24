module Page.Dashboard exposing (Model, Msg, init, update, view)

import Api.Endpoint
import Api.Http
import Api.Status exposing (Status(..))
import Data.Repository exposing (Repository)
import Html exposing (..)
import Html.Attributes exposing (..)
import Return exposing (..)
import Session exposing (Session)
import Task


type alias Model =
    { repos : Status (List Repository)
    }


type Msg
    = GotRepos (Result Api.Http.Error (List Repository))


init : Session -> Return Model Msg
init session =
    return
        { repos = Loading
        }
        |> withCommand
            (Api.Endpoint.getRepos session.credential
                |> Task.attempt GotRepos
            )


update : Msg -> Model -> Session -> Return Model Msg
update msg model session =
    case msg of
        GotRepos (Ok repos) ->
            return { model | repos = Loaded repos }

        GotRepos (Err error) ->
            return model
                |> withSession (Session.setError (Just error.message) session)


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
