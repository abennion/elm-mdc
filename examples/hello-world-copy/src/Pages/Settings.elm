module Pages.Settings exposing (ExternalMsg(..), Model, Msg(Mdc), init, update, view)

import Data.Session exposing (Session)
import Data.User as User exposing (User)
import Data.UserPhoto as UserPhoto
import Html exposing (Html, button, div, fieldset, h1, input, text, textarea)
import Html.Attributes exposing (attribute, class, defaultValue, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (decode, optional)
import Material
import Material.Button as Button
import Material.FormField as FormField
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Textfield as Textfield
import Material.Textfield.HelperText as Textfield
import Request.User exposing (storeSession)
import Route
import Util exposing ((=>), pair)
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form
import Views.Page as Page exposing (Context)


-- MODEL --


type alias Model m =
    { mdc : Material.Model m
    , errors : List Error
    , image : Maybe String
    , email : String
    , bio : String
    , username : String
    , password : Maybe String
    }


init : User -> Model m
init user =
    { mdc = Material.defaultModel
    , errors = []
    , image = UserPhoto.toMaybeString user.image
    , email = user.email
    , bio = Maybe.withDefault "" user.bio
    , username = User.usernameToString user.username
    , password = Nothing
    }



-- VIEW --


view : (Msg m -> m) -> Context m -> Model m -> Html m
view lift context model =
    let
        isLoading =
            context.isLoading

        session =
            context.session
    in
    div [ class "settings-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                    , Form.viewErrors model.errors
                    , viewForm lift context model
                    ]
                ]
            ]
        ]


viewForm : (Msg m -> m) -> Context m -> Model m -> Html m
viewForm lift context model =
    let
        isLoading =
            context.isLoading

        spinner isLoading =
            case isLoading of
                True ->
                    LinearProgress.view
                        [ LinearProgress.buffered 0.0 0.0
                        , LinearProgress.indeterminate
                        , cs "demo-linear-progress--custom"
                        ]
                        []

                False ->
                    LinearProgress.view
                        [ cs "demo-linear-progress--custom"
                        , cs "mdc-linear-progress--closed"
                        ]
                        []
    in
    context.body "Home"
        [ styled Html.div
            [ css "padding" "24px"
            ]
            [ styled Html.section
                (List.reverse
                    -- TODO: dang it
                    (cs "hero"
                        :: css "display" "-webkit-box"
                        :: css "display" "-ms-flexbox"
                        :: css "display" "flex"
                        :: css "-webkit-box-orient" "horizontal"
                        :: css "-webkit-box-direction" "normal"
                        :: css "-ms-flex-flow" "row nowrap"
                        :: css "flex-flow" "row nowrap"
                        :: css "-webkit-box-align" "center"
                        :: css "-ms-flex-align" "center"
                        :: css "align-items" "center"
                        :: css "-webkit-box-pack" "center"
                        :: css "-ms-flex-pack" "center"
                        :: css "justify-content" "center"
                        -- :: css "height" "360px"
                        :: css "min-height" "360px"
                        :: [ css "background-color" "rgba(0, 0, 0, 0.05)" ]
                    )
                )
                [ styled Html.div
                    [ css "width" "360px"
                    ]
                    [ spinner isLoading
                    , Html.div
                        []
                        [ Textfield.view (lift << Mdc)
                            "settings-image"
                            model.mdc
                            [ Textfield.label "URL of profile picture"
                            , Options.onInput (lift << SetImage)
                            ]
                            []
                        , Textfield.helperText
                            [ Textfield.persistent ]
                            []
                        ]
                    , Html.br [] []
                    , Html.div
                        []
                        [ Textfield.view (lift << Mdc)
                            "settings-username"
                            model.mdc
                            [ Textfield.label "Choose username"
                            , Options.onInput (lift << SetUsername)
                            , Textfield.value model.username
                            ]
                            []
                        , Textfield.helperText
                            [ Textfield.persistent
                            ]
                            []
                        ]
                    , Html.br [] []
                    , Button.view (lift << Mdc)
                        "login-submit"
                        model.mdc
                        [ Button.raised
                        , Options.onClick (lift SubmitForm)
                        ]
                        [ text "Submit"
                        ]
                    ]
                ]
            ]
        ]



--     Html.form [ onSubmit (lift SubmitForm) ]
--         [ fieldset []
--             [ Form.input
--                 [ placeholder "URL of profile picture"
--                 , defaultValue (Maybe.withDefault "" model.image)
--                 , onInput (lift << SetImage)
--                 ]
--                 []
--             , Form.input
--                 [ class "form-control-lg"
--                 , placeholder "Username"
--                 , defaultValue model.username
--                 , onInput (lift << SetUsername)
--                 ]
--                 []
--             , Form.textarea
--                 [ class "form-control-lg"
--                 , placeholder "Short bio about you"
--                 , attribute "rows" "8"
--                 , defaultValue model.bio
--                 , onInput (lift << SetBio)
--                 ]
--                 []
--             , Form.input
--                 [ class "form-control-lg"
--                 , placeholder "Email"
--                 , defaultValue model.email
--                 , onInput (lift << SetEmail)
--                 ]
--                 []
--             , Form.password
--                 [ class "form-control-lg"
--                 , placeholder "Password"
--                 , defaultValue (Maybe.withDefault "" model.password)
--                 , onInput (lift << SetPassword)
--                 ]
--                 []
--             , button
--                 [ class "btn btn-lg btn-primary pull-xs-right" ]
--                 [ text "Update Settings" ]
--             ]
--         ]
--     ]
-- ]
-- UPDATE --


type Msg m
    = Mdc (Material.Msg m)
    | SubmitForm
    | SetEmail String
    | SetUsername String
    | SetPassword String
    | SetBio String
    | SetImage String
    | SaveCompleted (Result Http.Error User)


type ExternalMsg
    = NoOp
    | SetUser User


update : (Msg m -> m) -> Session -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update lift session msg model =
    case msg of
        Mdc msg_ ->
            ( Material.update (lift << Mdc) msg_ model, NoOp )

        SubmitForm ->
            case validate modelValidator model of
                [] ->
                    session.user
                        |> Maybe.map .token
                        |> Request.User.edit model
                        |> Http.send (lift << SaveCompleted)
                        |> pair { model | errors = [] }
                        => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        SetEmail email ->
            { model | email = email }
                => Cmd.none
                => NoOp

        SetUsername username ->
            { model | username = username }
                => Cmd.none
                => NoOp

        SetPassword passwordStr ->
            let
                password =
                    if String.isEmpty passwordStr then
                        Nothing
                    else
                        Just passwordStr
            in
            { model | password = password }
                => Cmd.none
                => NoOp

        SetBio bio ->
            { model | bio = bio }
                => Cmd.none
                => NoOp

        SetImage imageStr ->
            let
                image =
                    if String.isEmpty imageStr then
                        Nothing
                    else
                        Just imageStr
            in
            { model | image = image }
                => Cmd.none
                => NoOp

        SaveCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to save changes" ]

                errors =
                    errorMessages
                        |> List.map (\errorMessage -> Form => errorMessage)
            in
            { model | errors = errors }
                => Cmd.none
                => NoOp

        SaveCompleted (Ok user) ->
            model
                => Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                => SetUser user



-- VALIDATION --


type Field
    = Form
    | Username
    | Email
    | Password
    | ImageUrl
    | Bio


type alias Error =
    ( Field, String )


modelValidator : Validator Error (Model m)
modelValidator =
    Validate.all
        [ ifBlank .username (Username => "username can't be blank.")
        , ifBlank .email (Email => "email can't be blank.")
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    decode (\email username password -> List.concat [ email, username, password ])
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (list (Decode.map errorToString string)) []
