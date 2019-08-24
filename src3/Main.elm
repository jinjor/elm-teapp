module Main exposing (main)

import Api.Http
import Browser
import Browser.Navigation as Nav
import Data.Credential exposing (Credential)
import Html exposing (..)
import Html.Attributes exposing (..)
import LocalStorage
import Page.Dashboard
import Page.Login
import Page.NotFound
import Return exposing (..)
import Route
import Session exposing (Session)
import Task
import Url exposing (Url)
import View.Layout



-- MAIN


type alias Flags =
    { credential : Maybe Credential }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { session : Session
    , error : Maybe Api.Http.Error
    , page : Page
    }


type Page
    = Blank
    | DashboardPage Page.Dashboard.Model
    | LoginPage Page.Login.Model
    | NotFoundPage Page.NotFound.Model
    | LogoutPage


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { credential } url key =
    { session = Session.init key url credential
    , error = Nothing
    , page = Blank
    }
        |> goTo url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SavedCredential (Maybe Credential)
    | GotError Api.Http.Error
    | DashboardMsg Page.Dashboard.Msg
    | LoginMsg Page.Login.Msg
    | NotFoundMsg Page.NotFound.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.session.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            goTo url model

        ( SavedCredential credential, _ ) ->
            goTo
                model.session.url
                { model | session = Session.setCredential credential model.session }

        ( GotError error, _ ) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        ( DashboardMsg subMsg, DashboardPage subModel ) ->
            Page.Dashboard.update subMsg subModel model.session
                |> toPair DashboardPage DashboardMsg model

        ( LoginMsg subMsg, LoginPage subModel ) ->
            Page.Login.update subMsg subModel model.session
                |> toPair LoginPage LoginMsg model

        ( NotFoundMsg subMsg, NotFoundPage subModel ) ->
            Page.NotFound.update subMsg subModel model.session
                |> toPair NotFoundPage NotFoundMsg model

        _ ->
            ( model, Cmd.none )


goTo : Url -> Model -> ( Model, Cmd Msg )
goTo url model =
    let
        session =
            model.session
                |> Session.setUrl url

        ( newModel, cmd ) =
            case ( session.credential, Route.parse url ) of
                ( Just credential, Just Route.Top ) ->
                    Page.Dashboard.init session
                        |> toPair DashboardPage DashboardMsg model

                ( Just credential, Nothing ) ->
                    Page.NotFound.init session
                        |> toPair NotFoundPage NotFoundMsg model

                ( Just credential, Just Route.Logout ) ->
                    ( { model | page = LogoutPage }, LocalStorage.saveCredential Nothing )

                ( Nothing, Just Route.Logout ) ->
                    ( { model | page = LogoutPage }, Nav.replaceUrl session.key "/" )

                ( Nothing, _ ) ->
                    Page.Login.init session
                        |> toPair LoginPage LoginMsg model
    in
    ( { model | error = Nothing }
    , cmd
    )


toPair :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> Return subModel subMsg
    -> ( Model, Cmd Msg )
toPair toPage toMsg mainModel { model, effects, session } =
    ( { mainModel
        | page = toPage model
        , session = Maybe.withDefault mainModel.session session
      }
    , effects
        |> List.map
            (\effect ->
                case effect of
                    Request task ->
                        Task.attempt
                            (\result ->
                                case result of
                                    Ok msg ->
                                        toMsg msg

                                    Err error ->
                                        GotError error
                            )
                            task

                    Command cmd ->
                        Cmd.map toMsg cmd
            )
        |> Cmd.batch
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.savedCredential SavedCredential



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "SPA Example"
    , body =
        View.Layout.view
            { loggedIn = model.session.credential /= Nothing
            , error = Maybe.map .message model.error
            , contents = viewPage model
            }
    }


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.page of
        Blank ->
            []

        DashboardPage subModel ->
            Page.Dashboard.view subModel model.session
                |> List.map (Html.map DashboardMsg)

        LoginPage subModel ->
            Page.Login.view subModel model.session
                |> List.map (Html.map LoginMsg)

        NotFoundPage subModel ->
            Page.NotFound.view subModel model.session
                |> List.map (Html.map NotFoundMsg)

        LogoutPage ->
            []
