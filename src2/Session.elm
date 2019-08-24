module Session exposing (Session, init, setCredential, setError, setUrl)

import Browser.Navigation as Nav
import Data.Credential exposing (Credential)
import Url exposing (Url)


type alias Session =
    { key : Nav.Key
    , url : Url
    , credential : Maybe Credential
    , error : Maybe String
    }


init : Nav.Key -> Url -> Maybe Credential -> Session
init key url credential =
    { key = key
    , url = url
    , credential = credential
    , error = Nothing
    }


setCredential : Maybe Credential -> Session -> Session
setCredential credential session =
    { session | credential = credential }


setUrl : Url -> Session -> Session
setUrl url session =
    { session | url = url }


setError : Maybe String -> Session -> Session
setError error session =
    { session | error = error }
