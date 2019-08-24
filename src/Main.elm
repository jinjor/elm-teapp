module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Data.Credential exposing (Credential)
import Html exposing (..)
import Html.Attributes exposing (..)
import LocalStorage
import Page.Dashboard
import Page.Login
import Page.NotFound
import Route
import Session exposing (Session)
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


type Model
    = Blank Session
    | DashboardPage Page.Dashboard.Model
    | LoginPage Page.Login.Model
    | NotFoundPage Page.NotFound.Model
    | LogoutPage Session


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { credential } url key =
    Blank (Session.init key url credential)
        |> goTo url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SavedCredential (Maybe Credential)
    | DashboardMsg Page.Dashboard.Msg
    | LoginMsg Page.Login.Msg
    | NotFoundMsg Page.NotFound.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (getSession model).key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            goTo url model

        ( SavedCredential credential, _ ) ->
            goTo
                (getSession model).url
                (updateSession (Session.setCredential credential) model)

        ( DashboardMsg subMsg, DashboardPage subModel ) ->
            Page.Dashboard.update subMsg subModel
                |> updateWith DashboardPage DashboardMsg model

        ( LoginMsg subMsg, LoginPage subModel ) ->
            Page.Login.update subMsg subModel
                |> updateWith LoginPage LoginMsg model

        ( NotFoundMsg subMsg, NotFoundPage subModel ) ->
            Page.NotFound.update subMsg subModel
                |> updateWith NotFoundPage NotFoundMsg model

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


getSession : Model -> Session
getSession model =
    case model of
        Blank session ->
            session

        DashboardPage { session } ->
            session

        LoginPage { session } ->
            session

        NotFoundPage { session } ->
            session

        LogoutPage session ->
            session


setSession : Session -> Model -> Model
setSession newSession model =
    case model of
        Blank _ ->
            Blank newSession

        DashboardPage subModel ->
            DashboardPage { subModel | session = newSession }

        LoginPage subModel ->
            LoginPage { subModel | session = newSession }

        NotFoundPage subModel ->
            NotFoundPage { subModel | session = newSession }

        LogoutPage _ ->
            LogoutPage newSession


updateSession : (Session -> Session) -> Model -> Model
updateSession f model =
    setSession (f (getSession model)) model


goTo : Url -> Model -> ( Model, Cmd Msg )
goTo url model =
    let
        session =
            Session.setUrl url (getSession model)
    in
    case ( session.credential, Route.parse url ) of
        ( Just credential, Just Route.Top ) ->
            Page.Dashboard.init session
                |> Tuple.mapBoth DashboardPage (Cmd.map DashboardMsg)

        ( Just credential, Nothing ) ->
            Page.NotFound.init session
                |> Tuple.mapBoth NotFoundPage (Cmd.map NotFoundMsg)

        ( Just credential, Just Route.Logout ) ->
            ( LogoutPage session, LocalStorage.saveCredential Nothing )

        ( Nothing, Just Route.Logout ) ->
            ( LogoutPage session, Nav.replaceUrl session.key "/" )

        ( Nothing, _ ) ->
            Page.Login.init session
                |> Tuple.mapBoth LoginPage (Cmd.map LoginMsg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.savedCredential SavedCredential



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "SPA Example"
    , body = viewPage model
    }


viewPage : Model -> List (Html Msg)
viewPage model =
    case model of
        Blank _ ->
            View.Layout.view
                { loggedIn = False
                , error = Nothing
                , contents = []
                }

        DashboardPage subModel ->
            Page.Dashboard.view subModel
                |> List.map (Html.map DashboardMsg)

        LoginPage subModel ->
            Page.Login.view subModel
                |> List.map (Html.map LoginMsg)

        NotFoundPage subModel ->
            Page.NotFound.view subModel
                |> List.map (Html.map NotFoundMsg)

        LogoutPage session ->
            View.Layout.view
                { loggedIn = False
                , error = Nothing
                , contents = []
                }
