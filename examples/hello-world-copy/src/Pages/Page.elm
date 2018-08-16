module Pages.Page exposing (Context, Page(..))

import Data.User as User exposing (User)
import Html exposing (Html)
import Route exposing (Route)


type Page
    = Blank
    | NotFound
    | Home
    | Login
    | Other


type alias Context m =
    { isLoading : Bool
    , setRoute : Maybe Route -> m
    , setUser : Maybe User -> m
    , user : Maybe User
    , body : String -> List (Html m) -> Html m
    }
