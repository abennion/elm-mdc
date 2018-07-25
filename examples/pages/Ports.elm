port module Ports exposing (onSessionChange, scrollTop, storeSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


port scrollTop : () -> Cmd msg



-- port setStorage : Model -> Cmd msg
