module Pages.Login exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Material
import Material.Button as Button
import Material.Checkbox as Checkbox
import Material.FormField as FormField
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Textfield as Textfield
import Material.Textfield.HelperText as Textfield
import Page exposing (Page)
import Route exposing (Route)
import Views.Form as Form


type Field
    = Form
    | Email
    | Password


{-| Recording validation errors on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFormErrors : Field -> List Error -> Html msg

...and it filters the list of errors to render only the ones for the given
Field. This way you can call this:

viewFormErrors Email model.errors

...next to the `email` field, and call `viewFormErrors Password model.errors`
next to the `password` field, and so on.

-}
type alias Error =
    ( Field, String )


type alias Model m =
    { mdc : Material.Model m
    , text : String
    , errors : List Error
    , email : String
    , password : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
    , errors = []
    , email = ""
    , password = ""
    }


type Msg m
    = Mdc (Material.Msg m)
    | Click String


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        Click text ->
            ( { model | text = text }
            , Cmd.none
            )


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    page.body "Login"
        page.isLoading
        [ styled Html.div
            [ css "padding" "24px"
            ]
            [ styled Html.h1 [] [ text "Sign in" ]
            , styled Html.p
                []
                [ Html.a
                    [ href "http://google.com" ]
                    [ text "Need an account?" ]
                ]
            , Form.viewErrors model.errors
            , viewForm lift page model
            ]
        ]


viewForm : (Msg m -> m) -> Page m -> Model m -> Html m
viewForm lift page model =
    FormField.view []
        [ Checkbox.view (lift << Mdc)
            "dialog-toggle-rtl"
            model.mdc
            [-- Checkbox.checked model.rtls
             -- , Options.onClick (lift ToggleRtl)
            ]
            []
        , Html.label []
            [ text "Toggle RTL"
            ]
        , Textfield.view (lift << Mdc)
            "my-password"
            model.mdc
            [ Textfield.label "Choose password"
            , Textfield.password
            , Textfield.pattern ".{8,}"
            , Textfield.required
            ]
            []
        , Textfield.helperText
            [ Textfield.persistent
            , Textfield.validationMsg
            ]
            [ Html.text "Must be at least 8 characters long"
            ]
        ]



-- Html.div []
--     [ Textfield.view (lift << Mdc) index model.mdc
--       [ Textfield.label "Choose password"
--       , Textfield.password
--       , Textfield.pattern ".{8,}"
--       , Textfield.required
--       , Textfield.disabled |> when state.disabled
--       , Textfield.dense |> when state.dense
--       ]
--       []
--     , Textfield.helperText
--       [ Textfield.persistent
--       , Textfield.validationMsg
--       ]
--       [ Html.text "Must be at least 8 characters long"
--       ]
--     ]
-- styled Html.form
--     [ Options.onSubmit SubmitForm
--     ]
--     [ styled Form.input
--         [ class "form-control-lg"
--         , placeholder "Email"
--         , Options.onInput SetEmail
--         ]
--         []
--     , styled Form.password
--         [ class "form-control-lg"
--         , placeholder "Password"
--         , Options.onInput SetPassword
--         ]
--         []
--     , button [ class "btn btn-lg btn-primary pull-xs-right" ]
--         [ text "Sign in" ]
--     ]
-- div [ class "auth-page" ]
--     [ div [ class "container page" ]
--         [ div [ class "row" ]
--             [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
--                 [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
--                 , p [ class "text-xs-center" ]
--                     [ a [ Route.href Route.Register ]
--                         [ text "Need an account?" ]
--                     ]
--                 , Form.viewErrors model.errors
--                 -- , viewForm
--                 ]
--             ]
--         ]
--     ]
-- view : (Msg m -> m) -> Page m -> Model m -> Html m
-- view lift page model =
--     let
--         fakeText =
--             "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
--                 ++ " eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut"
--                 ++ " enim ad minim veniam, quis nostrud exercitation ullamco laboris"
--                 ++ " nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in"
--                 ++ " reprehenderit in voluptate velit esse cillum dolore eu fugiat"
--                 ++ " nulla pariatur. Excepteur sint occaecat cupidatat non proident,"
--                 ++ " sunt in culpa qui officia deserunt mollit anim id est laborum."
--     in
--     page.body "Login"
--         page.isLoading
--         [ styled Html.div
--             []
--             [ styled Html.h2
--                 []
--                 [ text model.text
--                 ]
--             , styled Html.h2
--                 []
--                 [ text ("Is loading: " ++ toString page.isLoading)
--                 ]
--             , Button.view (lift << Mdc)
--                 "my-button"
--                 model.mdc
--                 [ Button.ripple
--                 , Options.onClick (page.navigate (Just Route.Home))
--                 ]
--                 [ text "Home!"
--                 ]
--             , styled Html.div
--                 [ css "padding" "24px"
--                 ]
--                 [ styled Html.p
--                     [ css "padding" "8px" ]
--                     [ text fakeText ]
--                 ]
--             ]
--         ]
