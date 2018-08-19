module Views.Page exposing (ActivePage(..), Context, bodyId)

import Data.Session exposing (Session)
import Data.User as User exposing (User, Username)
import Html exposing (Html)
import Route exposing (Route)


-- I don't like difference between Main.Page and ActivePage. Too many name
-- collisions.


type ActivePage
    = Other
    | Home
    | Login
    | Register
    | Settings
    | Profile Username
    | NewArticle


type alias Context m =
    { isLoading : Bool
    , setRoute : Maybe Route -> m
    , setUser : Maybe User -> m
    , session : Session
    , user : Maybe User
    , body : String -> List (Html m) -> Html m
    }


bodyId : String
bodyId =
    "page-body"
