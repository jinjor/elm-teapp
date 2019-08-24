module Page.NotFound exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)


type alias Model =
    {}


type Msg
    = NoOp


init : Session -> ( Model, Session, Cmd Msg )
init session =
    ( {}
    , session
    , Cmd.none
    )


update : Msg -> Model -> Session -> ( Model, Session, Cmd Msg )
update msg model session =
    case msg of
        NoOp ->
            ( model, session, Cmd.none )


view : Model -> Session -> List (Html Msg)
view model session =
    [ h1 [] [ text "NotFound" ] ]
