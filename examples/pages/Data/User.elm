module Data.User
    exposing
        ( Username
        , Users
        , decoder
        , encode
        , usernameDecoder
        , usernameParser
        , usernameToHtml
        )

-- import Data.AuthToken as AuthToken exposing (AuthToken)
-- import Data.UserPhoto as UserPhoto exposing (UserPhoto)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra


-- import UrlParser
-- import Util exposing ((=>))


type alias User =
    { email : String
    , username : Username
    , bio : Maybe String
    , createdAt : String
    , updatedAt : String
    }



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "email" Decode.string
        |> required "username" usernameDecoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "createdAt" Decode.string
        |> required "updatedAt" Decode.string


encode : User -> Value
encode user =
    Encode.object
        [ "email" => Encode.string user.email
        , "username" => encodeUsername user.username
        , "bio" => EncodeExtra.maybe Encode.string user.bio
        , "createdAt" => Encode.string user.createdAt
        , "updatedAt" => Encode.string user.updatedAt
        ]



-- IDENTIFIERS --


type Username
    = Username String


usernameToString : Username -> String
usernameToString (Username username) =
    username


usernameParser : UrlParser.Parser (Username -> a) a
usernameParser =
    UrlParser.custom "USERNAME" (Ok << Username)


usernameDecoder : Decoder Username
usernameDecoder =
    Decode.map Username Decode.string


encodeUsername : Username -> Value
encodeUsername (Username username) =
    Encode.string username


usernameToHtml : Username -> Html msg
usernameToHtml (Username username) =
    Html.text username
