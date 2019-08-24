module Session exposing (Session, init, setCredential, setUrl)

import Browser.Navigation as Nav
import Data.Credential exposing (Credential)
import Url exposing (Url)


type alias Session =
    { key : Nav.Key
    , url : Url
    , credential : Maybe Credential
    }


init : Nav.Key -> Url -> Maybe Credential -> Session
init key url credential =
    { key = key
    , url = url
    , credential = credential
    }


setCredential : Maybe Credential -> Session -> Session
setCredential credential session =
    { session | credential = credential }


setUrl : Url -> Session -> Session
setUrl url session =
    { session | url = url }
