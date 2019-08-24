module Page.NotFound exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Return exposing (..)
import Session exposing (Session)


type alias Model =
    {}


type Msg
    = NoOp


init : Session -> Return Model Msg
init session =
    return {}


update : Msg -> Model -> Session -> Return Model Msg
update msg model session =
    case msg of
        NoOp ->
            return model


view : Model -> Session -> List (Html Msg)
view model session =
    [ h1 [] [ text "NotFound" ] ]
