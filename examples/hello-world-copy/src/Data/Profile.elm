module Data.Profile exposing (Profile, decoder, defaultProfile)

import Data.User as User exposing (Username, stringToUsername)
import Data.UserPhoto as UserPhoto exposing (UserPhoto, maybeStringToUserPhoto)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Profile =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }


defaultProfile : Profile
defaultProfile =
    { username = stringToUsername ""
    , bio = Nothing
    , image = maybeStringToUserPhoto Nothing
    , following = False
    }


decoder : Decoder Profile
decoder =
    decode Profile
        |> required "username" User.usernameDecoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool
