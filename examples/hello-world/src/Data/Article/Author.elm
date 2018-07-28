module Data.Article.Author exposing (Author, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, required)
import User as User exposing (Username)
import UserPhoto as UserPhoto exposing (UserPhoto)


decoder : Decoder Author
decoder =
    decode Author
        |> required "username" User.usernameDecoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool


type alias Author =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }
