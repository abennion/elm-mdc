port module Pages.Login
    exposing
        ( Model
        , Msg(Mdc, SetUser)
        , defaultModel
        , subscriptions
        , update
        , view
        )

-- import Html.Attributes
-- import Html.Events as Html
-- import Material.Typography as Typography

import Data.User as User exposing (User, decoder)
import Html exposing (Html, text)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (decode, optional)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, onClick, styled, when)
import Material.Textfield as Textfield
import Material.Textfield.HelperText as Textfield
import Material.Theme as Theme
import Navigation
import Pages.Form as Form
import Pages.Page as Page exposing (Page)
import Pages.Url as Url
import Ports
import Request.User exposing (login, storeSession)
import Util exposing ((=>))


-- import Validate exposing (Validator, ifBlank, validate)
-- MODEL


type Field
    = Form
    | Email
    | Password


type alias Error =
    ( Field, String )


type alias Model m =
    { mdc : Material.Model m
    , errors : List Error
    , email : String
    , password : String
    , message : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , errors = []
    , email = ""
    , password = ""
    , message = ""
    }



-- UPDATE


type Msg m
    = Mdc (Material.Msg m)
    | SubmitForm
    | SetEmail String
    | SetPassword String
    | LoginCompleted (Result Http.Error User)
    | SetUser User



-- type ExternalMsg
--     = NoOp
--     | SetUser User


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case Debug.log "Todo.update.msg:" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        SubmitForm ->
            { model | errors = [] }
                => Http.send (lift << LoginCompleted) (Request.User.login model)

        -- case validate modelValidator model of
        --     [] ->
        --         { model | errors = [] }
        --             => Http.send LoginCompleted (Request.User.login model)
        --     errors ->
        --         { model | errors = errors }
        --             => Cmd.none
        SetEmail email ->
            { model | email = email }
                => Cmd.none

        SetPassword password ->
            { model | password = password }
                => Cmd.none

        LoginCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to perform login" ]
            in
            { model | errors = List.map (\errorMessage -> Form => errorMessage) errorMessages }
                => Cmd.none

        -- => NoOp
        LoginCompleted (Ok user) ->
            model
                => Cmd.batch
                    [ storeSession user
                    , Navigation.newUrl (Url.toString Url.Home)

                    -- TODO: update model.url?
                    -- , Route.modifyUrl Route.Home
                    ]

        SetUser user ->
            model ! []



-- => SetUser user
-- UpdateMessage msg_ ->
--     { model | message = msg_ } ! []
-- SaveEmail msg_ ->
--     ( model
--     , storeSession
--         (User model.message)
--     )
-- UpdateEmail msg_ ->
--     let
--         _ =
--             Debug.log "UpdateEmail" msg_
--         userText =
--             case msg_ of
--                 Just user ->
--                     user.email
--                 Nothing ->
--                     "not happenin'"
--     in
--     { model | email = userText } ! []
-- StoreText msg_ ->
-- modelValidator : Validator Error Model
-- modelValidator =
--     Validate.all
--         [ ifBlank .email (Email => "email can't be blank.")
--         , ifBlank .password (Password => "password can't be blank.")
--         ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    decode (\emailOrPassword email username password -> List.concat [ emailOrPassword, email, username, password ])
        |> optionalError "email or password"
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (Decode.list (Decode.map errorToString string)) []



-- VIEW


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    page.body "Login Page"
        [ styled Html.div
            [ css "padding" "16px"
            , css "margin" "0"
            , Theme.secondary
            , Theme.secondaryBg
            , Theme.textSecondaryOnLight
            ]
            [ text model.email
            ]
        , styled Html.section
            [ css "padding" "16px"
            ]
            [ Form.viewErrors model.errors
            , Html.form
                []
                [ styled Html.div
                    []
                    [ Textfield.view (lift << Mdc)
                        "my-text-field"
                        model.mdc
                        [ Textfield.label "Text field"
                        , Options.onInput (lift << SetEmail)
                        , css "background-color" "rgba(255, 255, 255, 0.1)"
                        ]
                        []
                    , Textfield.helperText
                        [ Textfield.persistent
                        ]
                        [ Html.text "Put some shit in here"
                        ]
                    ]
                , styled Html.div
                    []
                    [ Textfield.view (lift << Mdc)
                        "my-password-field"
                        model.mdc
                        [ Textfield.label "Choose password"
                        , Textfield.password
                        , Textfield.pattern ".{8,}"
                        , Textfield.required
                        , Options.onInput (lift << SetPassword)
                        , css "background-color" "rgba(255, 255, 255, 0.1)"
                        ]
                        []
                    , Textfield.helperText
                        [ Textfield.persistent
                        , Textfield.validationMsg
                        ]
                        [ Html.text "Must be at least 8 characters long"
                        ]
                    ]
                , styled Html.div
                    []
                    [ Button.view (lift << Mdc)
                        ("save-cookie" ++ "-baseline-button")
                        model.mdc
                        [ Button.raised
                        , Button.onClick (lift SubmitForm)
                        , Button.ripple
                        , css "margin" "16px"
                        ]
                        [ text "Submit" ]
                    ]
                ]
            ]
        ]


subscriptions : (Msg m -> m) -> Model m -> Sub m
subscriptions lift model =
    let
        _ =
            Debug.log "Todo.subscriptions" model
    in
    Sub.none



-- Sub.map (lift << UpdateEmail) sessionChange


sessionChange : Sub (Maybe User)
sessionChange =
    let
        _ =
            Debug.log "sessionChange" Ports.onSessionChange
    in
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)
