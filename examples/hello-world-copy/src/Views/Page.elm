module Views.Page exposing (Page)

import Data.User as User exposing (User, Username)
import Html exposing (Html, text)
import Route exposing (Route)


type alias Page m =
    { isLoading : Bool
    , setRoute : Maybe Route -> m
    , setUser : Maybe User -> m
    , user : Maybe User
    , body : String -> List (Html m) -> Html m
    }
