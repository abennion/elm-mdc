module Pages.Home exposing (Model, Msg(Mdc), defaultModel, init, update, view)

import Data.Article as Article exposing (Tag)
import Data.Session exposing (Session)
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Http
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Tabs as TabBar
import Material.Theme as Theme
import Pages.Errored exposing (PageLoadError, pageLoadError)
import Process
import Request.Article
import Route exposing (Route)
import SelectList exposing (SelectList)
import Task exposing (Task)
import Time
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


type Tab
    = Cats
    | Dogs


type TabState
    = TransitioningFrom Tab
    | Loaded Tab


type alias ErrorMsg =
    String


type alias Model m =
    { mdc : Material.Model m
    , text : String
    , tabState : TabState
    , tags : List Tag
    , feed : Feed.Model m
    }


type Msg m
    = Mdc (Material.Msg m)
    | Click String
    | SelectTab (Maybe Tab)
    | CatsLoaded (Result ErrorMsg (Model m))
    | DogsLoaded (Result ErrorMsg (Model m))
    | FeedMsg (Feed.Msg m)
    | SelectTag Tag



-- type alias InternalModel =
--     { errors : List String
--     , feed : Feed
--     , feedSources : SelectList FeedSource
--     , activePage : Int
--     , isLoading : Bool
--     }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
    , tabState = Loaded Cats
    , tags = []
    , feed = Feed.defaultModel
    }


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
    Task.map2 (Model Material.defaultModel "" (Loaded Cats)) loadTags loadSources
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
            -- ( { model | feed = newFeed }, Cmd.map (lift << FeedMsg) subCmd )
            ( { model | feed = newFeed }, subCmd )

        SelectTag tagName ->
            let
                subCmd =
                    Feed.selectTag (lift << FeedMsg) (Maybe.map .token session.user) tagName
            in
            -- ( model, Cmd.map (lift << FeedMsg) subCmd )
            ( model, subCmd )

        Click text ->
            ( { model | text = text }
            , Cmd.none
            )

        SelectTab maybeTab ->
            setTab lift maybeTab model

        CatsLoaded (Ok home) ->
            ( { model
                | tabState = Loaded Cats
              }
            , Cmd.none
            )

        CatsLoaded (Err error) ->
            ( model
            , Cmd.none
            )

        DogsLoaded (Ok home) ->
            ( { model
                | tabState = Loaded Dogs
              }
            , Cmd.none
            )

        DogsLoaded (Err error) ->
            ( model
            , Cmd.none
            )


setTab : (Msg m -> m) -> Maybe Tab -> Model m -> ( Model m, Cmd m )
setTab lift maybeTab model =
    let
        transition toMsg task =
            ( { model | tabState = TransitioningFrom (getTab model.tabState) }
            , Task.attempt toMsg task
            )
    in
    case Debug.log "setTab maybeTab" maybeTab of
        Nothing ->
            ( model, Cmd.none )

        Just Cats ->
            transition (lift << CatsLoaded)
                (Task.map
                    (\_ -> model)
                    (Process.sleep (Time.second * 2))
                )

        Just Dogs ->
            transition (lift << DogsLoaded)
                (Task.map
                    (\_ -> model)
                    (Process.sleep (Time.second * 2))
                )


getTab : TabState -> Tab
getTab tabState =
    case tabState of
        Loaded tab ->
            tab

        TransitioningFrom tab ->
            tab


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



-- VIEW


view : (Msg m -> m) -> Context m -> Model m -> Html m
view lift context model =
    case Debug.log "view model.tabState" model.tabState of
        Loaded tab ->
            viewPage lift context model False tab

        TransitioningFrom tab ->
            viewPage lift context model True tab


viewPage : (Msg m -> m) -> Context m -> Model m -> Bool -> Tab -> Html m
viewPage lift context model isLoading tab =
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
    context.body "Home"
        [ styled Html.div
            [ css "padding" "24px"
            ]
            [ styled Html.div
                [ Theme.secondary
                ]
                [ TabBar.view (lift << Mdc)
                    "my-tab-bar"
                    model.mdc
                    [ TabBar.indicator
                    , TabBar.scrolling
                    , Theme.secondary
                    ]
                    [ TabBar.tab
                        [ Options.onClick (lift (SelectTab (Just Cats)))
                        ]
                        [ text "Item One" ]
                    , TabBar.tab
                        [ Options.onClick (lift (SelectTab (Just Dogs)))
                        ]
                        [ text "Item Two" ]
                    , TabBar.tab [ Theme.secondary ] [ text "Item Three" ]
                    , TabBar.tab [] [ text "Item Four" ]
                    , TabBar.tab [] [ text "Item Five" ]
                    , TabBar.tab [] [ text "Item Six" ]
                    , TabBar.tab [] [ text "Item Seven" ]
                    , TabBar.tab [] [ text "Item Eight" ]
                    , TabBar.tab [] [ text "Item Nine" ]
                    ]
                ]
            , spinner isLoading
            , styled Html.h2
                []
                [ text model.text
                ]
            , styled Html.h2
                []
                [ text ("Is loading: " ++ toString context.isLoading)
                ]
            , styled Html.h2
                []
                [ case getTab model.tabState of
                    Cats ->
                        text "Cats"

                    Dogs ->
                        text "Dogs"
                ]
            , styled Html.div
                []
                [ styled Html.div
                    []
                    (viewFeed lift model model.feed)
                , styled Html.div
                    []
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
        -- [ Feed.viewFeedSources feed |> Html.map (lift << FeedMsg) ]
        -- :: (Feed.viewArticles feed |> List.map (Html.map (lift << FeedMsg)))
        [ Feed.viewFeedSources (lift << FeedMsg) feed ]
        :: Feed.viewArticles (lift << FeedMsg) feed


viewTags : (Msg m -> m) -> Model m -> List Tag -> Html m
viewTags lift model tags =
    styled Html.div
        [ cs "tag-list" ]
        (List.map (viewTag lift model) tags)


viewTag : (Msg m -> m) -> Model m -> Tag -> Html m
viewTag lift model tagName =
    Html.text (Article.tagToString tagName)



-- Button.view (lift << Mdc)
--                 "login-submit"
--                 model.mdc
--                 [ Button.raised
--                 , Options.onClick (lift SubmitForm)
--                 ]
--                 [ text "Submit"
--                 ]
--             ]
