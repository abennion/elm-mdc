module Pages.Home exposing (Model, Msg(Mdc), init, update, view)

import Data.Article as Article exposing (Tag)
import Data.Session exposing (Session)
import Html exposing (Html, div, text)
import Http
import Material
import Material.Chip as Chip
import Material.LayoutGrid as LayoutGrid
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Pages.Errored exposing (PageLoadError, pageLoadError)
import Request.Article
import SelectList exposing (SelectList)
import Task exposing (Task)
import Views.Article.Feed as Feed
    exposing
        ( FeedSource
        , defaultModel
        , globalFeed
        , tagFeed
        , yourFeed
        )
import Views.Page as Page exposing (Context)


-- MODEL


type alias Model m =
    { mdc : Material.Model m
    , choiceChip : Maybe Tag
    , tags : List Tag
    , feed : Feed.Model m
    }


type Msg m
    = Mdc (Material.Msg m)
    | FeedMsg (Feed.Msg m)
    | SelectTag Tag


init : Session -> Task PageLoadError (Model m)
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
    Task.map2 (Model Material.defaultModel Nothing) loadTags loadSources
        |> Task.mapError handleLoadError


update : (Msg m -> m) -> Msg m -> Session -> Model m -> ( Model m, Cmd m )
update lift msg session model =
    case Debug.log "Home.update msg" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        FeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update (lift << FeedMsg) session subMsg model.feed
            in
            ( { model | feed = newFeed }, subCmd )

        SelectTag tagName ->
            let
                subCmd =
                    Feed.selectTag (lift << FeedMsg) (Maybe.map .token session.user) tagName
            in
            ( { model | choiceChip = Just tagName }, subCmd )



-- VIEW


view : (Msg m -> m) -> Context m -> Model m -> Html m
view lift context model =
    viewPage lift context model False


viewPage : (Msg m -> m) -> Context m -> Model m -> Bool -> Html m
viewPage lift context model isLoading =
    let
        session =
            context.session

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
    -- LayoutGrid.view []
    --     [ LayoutGrid.cell
    --           [ LayoutGrid.span6
    --           , LayoutGrid.span8Tablet
    --           ]
    --           []
    --     , LayoutGrid.cell
    --           [ LayoutGrid.span4
    --           , LayoutGrid.span6Tablet
    --           ]
    --           []
    --     , LayoutGrid.cell
    --           [ LayoutGrid.span2
    --           , LayoutGrid.span4Phone
    --           ]
    --           []
    --     ]
    context.body "Home"
        [ styled Html.div
            [ css "padding" "24px"
            ]
            [ LayoutGrid.view
                []
                [ LayoutGrid.cell
                    [ LayoutGrid.span10
                    ]
                    [ styled Html.div
                        [ cs "mdc-theme--on-surface" ]
                        [ styled Html.div
                            []
                            (viewFeed lift model model.feed)
                        ]
                    ]
                , LayoutGrid.cell
                    [ LayoutGrid.span2
                    ]
                    [ styled Html.div
                        []
                        [ Html.p
                            []
                            [ text "Popular Tags" ]
                        , viewTags lift model model.tags
                        ]
                    ]
                ]
            ]
        ]


viewFeed : (Msg m -> m) -> Model m -> Feed.Model m -> List (Html m)
viewFeed lift model feed =
    styled Html.div
        [ cs "feed-toggle" ]
        [ Feed.viewFeedSources (lift << FeedMsg) feed ]
        :: Feed.viewArticles (lift << FeedMsg) feed


viewTags : (Msg m -> m) -> Model m -> List Tag -> Html m
viewTags lift model tags =
    styled Html.div
        [ cs "tag-list" ]
        [ Chip.chipset
            [ Chip.choice
            ]
            (List.map (viewTag lift model) tags)
        ]


viewTag : (Msg m -> m) -> Model m -> Tag -> Html m
viewTag lift model tagName =
    let
        tag =
            Article.tagToString tagName

        index =
            "chip-" ++ tag

        _ =
            Debug.log "isSelected" (Just tagName == model.choiceChip)
    in
    Chip.view (lift << Mdc)
        index
        model.mdc
        [ Chip.onClick (lift (SelectTag tagName))
        , when (Just tagName == model.choiceChip) Chip.selected
        , Chip.checkmark
        ]
        [ text tag
        ]
