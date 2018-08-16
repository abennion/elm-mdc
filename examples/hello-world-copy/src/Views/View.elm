module Views.View exposing (View)

import Data.User as User exposing (User)
import Route exposing (Route)


type alias View m =
    { isLoading : Bool
    , setRoute : Maybe Route -> m
    , setUser : Maybe User -> m
    }
