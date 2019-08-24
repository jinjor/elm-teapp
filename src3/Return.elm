module Return exposing (Effect(..), Return, return, withCommand, withRequest, withSession)

import Api.Http
import Session exposing (Session)
import Task exposing (Task)


type alias Return model msg =
    { model : model
    , effects : List (Effect msg)
    , session : Maybe Session
    }


type Effect msg
    = Request (Task Api.Http.Error msg)
    | Command (Cmd msg)


return : model -> Return model msg
return model =
    { model = model
    , effects = []
    , session = Nothing
    }


withRequest : Task Api.Http.Error msg -> Return model msg -> Return model msg
withRequest task ret =
    { ret
        | effects = ret.effects ++ [ Request task ]
    }


withCommand : Cmd msg -> Return model msg -> Return model msg
withCommand cmd ret =
    { ret
        | effects = ret.effects ++ [ Command cmd ]
    }


withSession : Session -> Return model msg -> Return model msg
withSession session ret =
    { ret
        | session = Just session
    }
