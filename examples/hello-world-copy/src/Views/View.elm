module Views.View exposing (Context)

import Data.User as User exposing (User)
import Pages.Page exposing (Page)
import Route exposing (Route)


type alias Context m =
    { page : Page
    , isLoading : Bool
    , setRoute : Maybe Route -> m
    , setUser : Maybe User -> m
    , user : Maybe User
    , title : String
    }
