module View.NavBar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route


view : Bool -> Maybe String -> Html msg
view loggedIn maybeError =
    nav
        [ class "navbar" ]
        [ a
            [ class "navbar-brand"
            , href (Route.toPath Route.Top)
            ]
            [ text "SPA Example" ]
        , if loggedIn then
            menuItem (a [ href (Route.toPath Route.Logout) ] [ text "Logout" ])

          else
            menuItem (a [ href (Route.toPath Route.Top) ] [ text "Login" ])
        , case maybeError of
            Just error ->
                viewMessage error

            Nothing ->
                text ""
        ]


menuItem : Html msg -> Html msg
menuItem html =
    div [ class "navbar-item" ] [ html ]


viewMessage : String -> Html msg
viewMessage error =
    div [ class "navbar-message" ] [ text error ]
