module View.Layout exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import View.NavBar


type alias Options msg =
    { loggedIn : Bool
    , error : Maybe String
    , contents : List (Html msg)
    }


view : Options msg -> List (Html msg)
view { loggedIn, error, contents } =
    [ View.NavBar.view loggedIn error
    , div [ class "main" ] contents
    ]
