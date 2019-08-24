module Page.NotFound exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import View.Layout


type alias Model =
    { session : Session }


type Msg
    = NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> List (Html Msg)
view model =
    View.Layout.view
        { loggedIn = model.session.credential /= Nothing
        , error = Nothing
        , contents = [ h1 [] [ text "NotFound" ] ]
        }
