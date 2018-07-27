module Pages.Home exposing (Model, Msg(Mdc), defaultModel, update, view)

-- import Data.Article as Article exposing (Tag)
-- import Data.Session exposing (Session)
-- import Request.Article

import Html exposing (Html, text)
import Http
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Textfield as Textfield
import Material.Theme as Theme
import Material.Typography as Typography
import Pages.Page as Page exposing (Page)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation)


-- import Views.Article.Feed as Feed exposing (FeedSource, globalFeed, tagFeed, yourFeed)


type alias Model m =
    { mdc : Material.Model m

    -- , tags : List Tag
    -- , feed : Feed.Model
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    }



-- init : Session -> Task PageLoadError Model
-- init session =
--     let
--         feedSources =
--             if session.user == Nothing then
--                 SelectList.singleton globalFeed
--             else
--                 SelectList.fromLists [] yourFeed [ globalFeed ]
--         loadTags =
--             Request.Article.tags
--                 |> Http.toTask
--         loadSources =
--             Feed.init session feedSources
--         handleLoadError _ =
--             pageLoadError Page.Home "Homepage is currently unavailable."
--     in
--     Task.map2 Model loadTags loadSources
--         |> Task.mapError handleLoadError


type Msg m
    = Mdc (Material.Msg m)
    | UpdateTextMsg String



-- UPDATE


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        UpdateTextMsg msg_ ->
            model ! []



-- VIEW


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    let
        _ =
            Nothing
    in
    page.body "Home"
        [ styled Html.section
            []
            [ styled Html.div
                [ css "margin" "24px"
                , css "margin-top" "0"
                , css "margin-bottom" "16px"
                ]
                [ LinearProgress.view
                    [ LinearProgress.buffered 0.0 0.0
                    , Theme.secondary
                    ]
                    []
                ]
            ]
        , styled Html.section
            []
            [ styled Html.div
                []
                [ text "Nothing yet"
                ]
            ]
        ]
