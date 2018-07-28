module Page.Home
    exposing
        ( Model
        , Msg(Mdc)
        , defaultModel
        , init
        , update
        , view
        )

import Html exposing (Html, text)
import Material
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Typography as Typography
import Page.Errored
import Page.Page exposing (Page)


type alias Model m =
    { mdc : Material.Model m
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    }


type Msg m
    = Mdc (Material.Msg m)


init : Session -> Task PageLoadError Model
init session =
    let
        feedSources =
            if session.user == Nothing then
                SelectList.singleton globalFeed
            else
                SelectList.fromLists [] yourFeed [ globalFeed ]

        loadTags =
            Request.Article.tags
                |> Http.toTask

        loadSources =
            Feed.init session feedSources

        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
    Task.map2 Model loadTags loadSources
        |> Task.mapError handleLoadError


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model



-- type alias Page m =
--     { toolbar : String -> Html m
--     , fixedAdjust : Options.Property () m
--     , navigate : Url -> m
--     , body : String -> List (Html m) -> Html m
--     }


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    styled Html.div
        []
        [ text "Home!"
        ]
