module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.Article as Article
import Data.User as User exposing (Username)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | Root
    | Login
    | Other
    | Profile Username
    | Article Article.Slug
    | NewArticle
    | EditArticle Article.Slug
    | Register


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Other (s "other")
        , Url.map Register (s "register")
        , Url.map NewArticle (s "editor")
        , Url.map EditArticle (s "editor" </> Article.slugParser)
        , Url.map Profile (s "profile" </> User.usernameParser)
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Other ->
                    [ "other" ]

                Register ->
                    [ "register" ]

                Article slug ->
                    [ "article", Article.slugToString slug ]

                Profile username ->
                    [ "profile", User.usernameToString username ]

                NewArticle ->
                    [ "editor" ]

                EditArticle slug ->
                    [ "editor", Article.slugToString slug ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Root
    else
        parseHash route location
