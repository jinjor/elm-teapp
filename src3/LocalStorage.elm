port module LocalStorage exposing (saveCredential, savedCredential)

import Data.Credential exposing (Credential)


port saveCredential : Maybe Credential -> Cmd msg


port savedCredential : (Maybe Credential -> msg) -> Sub msg
