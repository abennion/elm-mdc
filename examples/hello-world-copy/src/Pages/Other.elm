module Pages.Other exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, styled, when)
import Page exposing (Page)
import Route exposing (Route)


type alias Model m =
    { mdc : Material.Model m
    , text : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
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
    page.body "Other"
        [ styled Html.div
            []
            [ styled Html.h2
                []
                [ text model.text
                ]
            , styled Html.h2
                []
                [ text ("Is loading: " ++ toString page.isLoading)
                ]
            , Button.view (lift << Mdc)
                "my-button"
                model.mdc
                [ Button.ripple
                , Options.onClick (page.navigate (Just Route.Home))
                ]
                [ text "Home!"
                ]
            ]
        ]
