module Pages.Home exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, styled, when)
import Page exposing (Page)
import Pages.Errored exposing (PageLoadError, pageLoadError)
import Route exposing (Route)
import Task exposing (Task)


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
    page.body "Home"
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
                , Options.onClick (page.navigate (Just Route.Other))
                ]
                [ text "Other!"
                ]
            ]
        ]



-- init : Route -> Task PageLoadError Model m
-- init route =
--     let
--         loadSources =
--             Feed.init session feedSources
--         handleLoadError _ =
--             pageLoadError Route.Home "Homepage is currently unavailable."
--     in
--     Task.map Model loadSources
--         |> Task.mapError handleLoadError
--
--
-- -- we don't want Task.map2 here...
-- Task.attempt (\ -> Success (Model defaultModel.mdc "ain't nothing here"))
--     |> Task.mapError handleLoadError
-- | HomeLoaded (Result PageLoadError Home.Model)
-- Task.map HomeLoaded
--     (Ok "something something")
--     (Model defaultModel.mdc "ain't nothing here")
