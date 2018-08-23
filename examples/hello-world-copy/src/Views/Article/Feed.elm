module Views.Article.Feed exposing (FeedSource, Model, Msg, authorFeed, defaultModel, favoritedFeed, globalFeed, init, selectTag, tagFeed, update, viewArticles, viewFeedSources, yourFeed)

{-| NOTE: This module has its own Model, view, and update. This is not normal!
If you find yourself doing this often, please watch <https://www.youtube.com/watch?v=DoA4Txr4GUs>

This is the reusable Article Feed that appears on both the Home page as well as
on the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}

import Data.Article as Article exposing (Article, Tag)
import Data.Article.Feed exposing (Feed)
import Data.AuthToken exposing (AuthToken)
import Data.Session exposing (Session)
import Data.User as User exposing (Username)
import Data.UserPhoto as UserPhoto exposing (UserPhoto, photoToUrl)
import Dom.Scroll
import Html exposing (..)
import Http
import Material
import Material.LinearProgress as LinearProgress
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Tabs as TabBar
import Material.Theme as Theme
import Request.Article
import SelectList exposing (Position(..), SelectList)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation, pair, viewIf)
import Views.Article
import Views.Errors as Errors
import Views.Page exposing (bodyId)
import Views.Spinner exposing (spinner)


-- MODEL --


type Model m
    = Model (InternalModel m)


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this file.
-}
type alias InternalModel m =
    { mdc : Material.Model m
    , errors : List String
    , feed : Feed
    , feedSources : SelectList FeedSource
    , activePage : Int
    , isLoading : Bool
    }


defaultModel : Model m
defaultModel =
    Model
        (InternalModel
            Material.defaultModel
            []
            (Feed [] 0)
            (SelectList.singleton globalFeed)
            0
            False
        )


init : Session -> SelectList FeedSource -> Task Http.Error (Model m)
init session feedSources =
    let
        source =
            SelectList.selected feedSources

        toModel ( activePage, feed ) =
            Model
                { mdc = Material.defaultModel
                , errors = []
                , activePage = activePage
                , feed = feed
                , feedSources = feedSources
                , isLoading = False
                }
    in
    source
        |> fetch (Maybe.map .token session.user) 1
        |> Task.map toModel



-- VIEW
-- viewArticles lift (Model { mdc, activePage, feed, feedSources }) =


viewArticles : (Msg m -> m) -> Model m -> List (Html m)
viewArticles lift model =
    let
        _ =
            Debug.log "Feed.viewArticles" ""

        (Model internalModel) =
            model

        activePage =
            internalModel.activePage

        feed =
            internalModel.feed

        feedSources =
            internalModel.feedSources

        articleList items =
            Lists.ul
                [ Lists.avatarList
                ]
                items
    in
    [ articleList (List.map (viewArticle lift internalModel (lift << ToggleFavorite)) feed.articles) ]
        ++ [ pagination lift activePage feed (SelectList.selected feedSources) ]



-- div
--     [ class "article-preview" ]
--     [ div [ class "article-meta" ]
--         [ a [ Route.href (Route.Profile author.username) ]
--             [ img [ UserPhoto.src author.image ] [] ]
--         , div [ class "info" ]
--             [ Views.Author.view author.username
--             , viewTimestamp article
--             ]
--         , Favorite.button
--             toggleFavorite
--             article
--             [ class "pull-xs-right" ]
--             [ text (" " ++ toString article.favoritesCount) ]
--         ]
--     , a [ class "preview-link", Route.href (Route.Article article.slug) ]
--         [ h1 [] [ text article.title ]
--         , p [] [ text article.description ]
--         , span [] [ text "Read more..." ]
--         ]
--     ]


viewArticle : (Msg m -> m) -> InternalModel m -> (Article a -> m) -> Article a -> Html m
viewArticle lift model toggleFavorite article =
    let
        author =
            article.author

        item src primary secondary icon =
            let
                -- url =
                --     "images/" ++ src
                url =
                    src

                article_ =
                    { article | body = () }

                icon =
                    case article.favorited of
                        True ->
                            "favorite"

                        False ->
                            "favorite_border"
            in
            -- so, in a list, you click on the entire thing, not individual
            -- parts, so lets try using something else.
            Lists.li
                [ Options.onClick (lift (ToggleFavorite article_))
                ]
                [ Lists.graphicImage [] url
                , Lists.text []
                    [ Html.text primary
                    , Lists.secondaryText []
                        [ Html.text secondary
                        ]
                    ]
                , Lists.metaIcon
                    [ css "text-decoration" "none"
                    , css "color" "#ff4081"
                    ]
                    icon
                ]
    in
    -- item "animal3.svg" "Brown Bear" "Jan 9, 2014" "favorite"
    -- item "animal1.svg" "Panda" "Jan 9, 2014" "favorite_border"
    item (UserPhoto.photoToUrl author.image)
        (User.usernameToString author.username)
        (toString article.createdAt)
        "favorite"


viewFeedSources : (Msg m -> m) -> Model m -> Html m
viewFeedSources lift (Model internalModel) =
    let
        _ =
            Debug.log "Feed.viewFeedSources" ""

        feedSources =
            internalModel.feedSources

        isLoading =
            internalModel.isLoading

        errors =
            internalModel.errors

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
    -- ul [ class "nav nav-pills outline-active" ] <|
    --     SelectList.toList (SelectList.mapBy (viewFeedSource lift) feedSources)
    --         ++ [ Errors.view (lift DismissErrors) errors, viewIf isLoading spinner ]
    styled Html.div
        []
        [ TabBar.view (lift << Mdc)
            "my-tab-bar"
            internalModel.mdc
            [ TabBar.indicator
            , TabBar.scrolling
            , Theme.secondary
            ]
            (SelectList.toList
                (SelectList.mapBy (viewFeedSource lift) feedSources)
            )
        , spinner isLoading
        ]


viewFeedSource : (Msg m -> m) -> Position -> FeedSource -> TabBar.Tab m
viewFeedSource lift position source =
    let
        _ =
            Debug.log "Feed.viewFeedSource" ""
    in
    TabBar.tab
        [ Options.onClick (lift (SelectFeedSource source))
        ]
        [ text (sourceName source) ]


selectTag : (Msg m -> m) -> Maybe AuthToken -> Tag -> Cmd m
selectTag lift maybeAuthToken tagName =
    let
        source =
            tagFeed tagName
    in
    source
        |> fetch maybeAuthToken 1
        |> Task.attempt (lift << FeedLoadCompleted source)


sourceName : FeedSource -> String
sourceName source =
    case source of
        YourFeed ->
            "Your Feed"

        GlobalFeed ->
            "Global Feed"

        TagFeed tagName ->
            "#" ++ Article.tagToString tagName

        FavoritedFeed username ->
            "Favorited Articles"

        AuthorFeed username ->
            "My Articles"


limit : FeedSource -> Int
limit feedSource =
    case feedSource of
        YourFeed ->
            10

        GlobalFeed ->
            10

        TagFeed tagName ->
            10

        FavoritedFeed username ->
            5

        AuthorFeed username ->
            5


pagination : (Msg m -> m) -> Int -> Feed -> FeedSource -> Html m
pagination lift activePage feed feedSource =
    let
        articlesPerPage =
            limit feedSource

        totalPages =
            ceiling (toFloat feed.articlesCount / toFloat articlesPerPage)
    in
    styled Html.p
        []
        [ if totalPages > 1 then
            List.range 1 totalPages
                |> List.map (\page -> pageLink lift page (page == activePage))
                |> styled ul
                    [ cs "pagination"
                    , css "list-style" "none"
                    ]
          else
            Html.text ""
        ]


pageLink : (Msg m -> m) -> Int -> Bool -> Html m
pageLink lift page isActive =
    let
        _ =
            Debug.log "isActive" (toString isActive ++ " " ++ toString page)
    in
    styled li
        [ cs "page-item"
        , when (isActive == True) (css "border-bottom" "2px solid #fff")
        , css "display" "inline"
        , css "padding" "4px"
        ]
        [ styled a
            [ cs "page-link"
            , Options.onClick (lift (SelectPage page))
            ]
            [ text (toString page) ]
        ]



-- UPDATE --


type Msg m
    = Mdc (Material.Msg m)
    | DismissErrors
    | SelectFeedSource FeedSource
    | FeedLoadCompleted FeedSource (Result Http.Error ( Int, Feed ))
    | ToggleFavorite (Article ())
    | FavoriteCompleted (Result Http.Error (Article ()))
    | SelectPage Int


update : (Msg m -> m) -> Session -> Msg m -> Model m -> ( Model m, Cmd m )
update lift session msg (Model internalModel) =
    updateInternal lift session msg internalModel
        |> Tuple.mapFirst Model


updateInternal : (Msg m -> m) -> Session -> Msg m -> InternalModel m -> ( InternalModel m, Cmd m )
updateInternal lift session msg model =
    case Debug.log "Feed.updateInternal msg" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        DismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        SelectFeedSource source ->
            source
                |> fetch (Maybe.map .token session.user) 1
                |> Task.attempt (lift << FeedLoadCompleted source)
                |> pair { model | isLoading = True }

        FeedLoadCompleted source (Ok ( activePage, feed )) ->
            { model
                | feed = feed
                , feedSources = selectFeedSource source model.feedSources
                , activePage = activePage
                , isLoading = False
            }
                => Cmd.none

        FeedLoadCompleted _ (Err error) ->
            ( { model
                | errors = model.errors ++ [ "Server error while trying to load feed" ]
                , isLoading = False
              }
            , Cmd.none
            )

        ToggleFavorite article ->
            case Debug.log "Feed.ToggleFavorite session.user" session.user of
                Nothing ->
                    ( { model | errors = model.errors ++ [ "You are currently signed out. You must sign in to favorite articles." ] }
                    , Cmd.none
                    )

                Just user ->
                    Request.Article.toggleFavorite article user.token
                        |> Http.send (lift << FavoriteCompleted)
                        |> pair model

        FavoriteCompleted (Ok article) ->
            let
                feed =
                    model.feed

                newFeed =
                    { feed | articles = List.map (replaceArticle article) feed.articles }
            in
            { model | feed = newFeed } => Cmd.none

        FavoriteCompleted (Err error) ->
            { model | errors = model.errors ++ [ "Server error while trying to favorite article." ] }
                => Cmd.none

        SelectPage page ->
            let
                source =
                    SelectList.selected model.feedSources
            in
            source
                |> fetch (Maybe.map .token session.user) page
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt (lift << FeedLoadCompleted source)
                |> pair { model | isLoading = True }


scrollToTop : Task x ()
scrollToTop =
    Dom.Scroll.toTop bodyId
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())


fetch : Maybe AuthToken -> Int -> FeedSource -> Task Http.Error ( Int, Feed )
fetch token page feedSource =
    let
        defaultListConfig =
            Request.Article.defaultListConfig

        articlesPerPage =
            limit feedSource

        offset =
            (page - 1) * articlesPerPage

        listConfig =
            { defaultListConfig | offset = offset, limit = articlesPerPage }

        task =
            case feedSource of
                YourFeed ->
                    let
                        defaultFeedConfig =
                            Request.Article.defaultFeedConfig

                        feedConfig =
                            { defaultFeedConfig | offset = offset, limit = articlesPerPage }
                    in
                    token
                        |> Maybe.map (Request.Article.feed feedConfig >> Http.toTask)
                        |> Maybe.withDefault (Task.fail (Http.BadUrl "You need to be signed in to view your feed."))

                GlobalFeed ->
                    Request.Article.list listConfig token
                        |> Http.toTask

                TagFeed tagName ->
                    Request.Article.list { listConfig | tag = Just tagName } token
                        |> Http.toTask

                FavoritedFeed username ->
                    Request.Article.list { listConfig | favorited = Just username } token
                        |> Http.toTask

                AuthorFeed username ->
                    Request.Article.list { listConfig | author = Just username } token
                        |> Http.toTask
    in
    task
        |> Task.map (\feed -> ( page, feed ))


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if newArticle.slug == oldArticle.slug then
        newArticle
    else
        oldArticle


selectFeedSource : FeedSource -> SelectList FeedSource -> SelectList FeedSource
selectFeedSource source sources =
    let
        withoutTags =
            sources
                |> SelectList.toList
                |> List.filter (not << isTagFeed)

        newSources =
            case source of
                YourFeed ->
                    withoutTags

                GlobalFeed ->
                    withoutTags

                FavoritedFeed _ ->
                    withoutTags

                AuthorFeed _ ->
                    withoutTags

                TagFeed _ ->
                    withoutTags ++ [ source ]
    in
    case newSources of
        [] ->
            -- This should never happen. If we had a logging service set up,
            -- we would definitely want to report if it somehow did happen!
            sources

        first :: rest ->
            SelectList.fromLists [] first rest
                |> SelectList.select ((==) source)


isTagFeed : FeedSource -> Bool
isTagFeed source =
    case source of
        TagFeed _ ->
            True

        _ ->
            False



-- FEEDSOURCE --


type FeedSource
    = YourFeed
    | GlobalFeed
    | TagFeed Tag
    | FavoritedFeed Username
    | AuthorFeed Username


yourFeed : FeedSource
yourFeed =
    YourFeed


globalFeed : FeedSource
globalFeed =
    GlobalFeed


tagFeed : Tag -> FeedSource
tagFeed =
    TagFeed


favoritedFeed : Username -> FeedSource
favoritedFeed =
    FavoritedFeed


authorFeed : Username -> FeedSource
authorFeed =
    AuthorFeed
