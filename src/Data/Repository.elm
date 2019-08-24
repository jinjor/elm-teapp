module Data.Repository exposing (Repository, decoder)

import Json.Decode exposing (..)


type alias Repository =
    { name : String
    , description : Maybe String
    , language : Maybe String
    , owner : String
    , fork : Int
    , star : Int
    , watch : Int
    }


decoder : Decoder Repository
decoder =
    map7 Repository
        (field "name" string)
        (maybe (field "description" string))
        (maybe (field "language" string))
        (at [ "owner", "login" ] string)
        (field "forks_count" int)
        (field "stargazers_count" int)
        (field "watchers_count" int)
