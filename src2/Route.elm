module Route exposing (Route(..), parse, toPath)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Top
    | Logout


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    oneOf
        [ map Top top
        , map Logout (s "logout")
        ]


toPath : Route -> String
toPath route =
    case route of
        Top ->
            "/"

        Logout ->
            "/logout"
