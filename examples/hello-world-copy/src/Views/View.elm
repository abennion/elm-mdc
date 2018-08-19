module Views.View exposing (Context)

import Data.User as User exposing (User)
import Route exposing (Route)
import Views.Page exposing (ActivePage)


type alias Context m =
    { page : ActivePage
    , isLoading : Bool
    , setRoute : Maybe Route -> m
    , setUser : Maybe User -> m
    , user : Maybe User
    , title : String
    }
