module Main exposing (..)

import Data.Article exposing (Slug)
import Data.Session exposing (Session)
import Data.User as User exposing (User, Username)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, styled)
import Material.Typography as Typography
import Navigation exposing (Location)
import Pages.Article as Article
import Pages.Article.Editor as Editor
import Pages.Errored as Errored exposing (PageLoadError)
import Pages.Home exposing (Model, Msg(Mdc), init, update, view)
import Pages.Login exposing (Model, Msg(..), defaultModel, update, view)
import Pages.Other exposing (Model, Msg(Mdc), defaultModel, update, view)
import Pages.Profile exposing (Model, Msg(Mdc), defaultModel, init, update, view)
import Ports exposing (..)
import Process
import Route exposing (Route)
import Task
import Time
import Views.Drawer exposing (Model, Msg(..), defaultModel, update, view)
import Views.Page exposing (ActivePage, Context)
import Views.Toolbar exposing (Model, Msg(..), defaultModel, update, view)
import Views.View as View exposing (Context)


{-| TODO

    - take a closer look at this subModel thing...
    - Pass messages from components without exposing Msg values?
        Use an external message instead?
    - PageLoadError
    - Can we do a persistent drawer with the top app bar?
    - How to do proper formatting of main body under top app bar?

-}



-- MODEL


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home (Pages.Home.Model Msg)
    | Settings
    | Login
    | Register
    | Profile Username (Pages.Profile.Model Msg)
    | Article
    | Editor (Maybe Slug)
    | Other


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias ErrorMsg =
    String


type alias Model =
    { mdc : Material.Model Msg
    , session : Session
    , pageState : PageState
    , error : ErrorMsg
    , toolbar : Views.Toolbar.Model Msg
    , drawer : Views.Drawer.Model Msg
    , login : Pages.Login.Model Msg
    , other : Pages.Other.Model Msg
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , session = { user = Nothing }
    , pageState = Loaded Blank
    , error = "nothing"
    , toolbar = Views.Toolbar.defaultModel
    , drawer = Views.Drawer.defaultModel
    , login = Pages.Login.defaultModel
    , other = Pages.Other.defaultModel
    }


type Msg
    = Mdc (Material.Msg Msg)
    | SetRoute (Maybe Route)
    | SetUser (Maybe User)
    | ToolbarMsg (Views.Toolbar.Msg Msg)
    | DrawerMsg (Views.Drawer.Msg Msg)
    | HomeMsg (Pages.Home.Msg Msg)
    | HomeLoaded (Result PageLoadError (Pages.Home.Model Msg))
    | LoginMsg (Pages.Login.Msg Msg)
    | OtherMsg (Pages.Other.Msg Msg)
    | OtherLoaded (Result ErrorMsg (Pages.Other.Model Msg))
    | Click
    | ArticleLoaded (Result PageLoadError Article.Model)
    | ProfileMsg (Pages.Profile.Msg Msg)
    | ProfileLoaded Username (Result PageLoadError (Pages.Profile.Model Msg))
    | EditArticleLoaded Slug (Result PageLoadError Editor.Model)



-- MAIN


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { defaultModel | session = { user = decodeUserFromJson val } }


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        page =
            getPage model.pageState

        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }
            , Cmd.map toMsg newCmd
            )

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( Mdc msg_, _ ) ->
            Material.update Mdc msg_ model

        ( SetRoute route, _ ) ->
            setRoute route model

        ( SetUser user, _ ) ->
            let
                cmd =
                    if session.user /= Nothing && user == Nothing then
                        Route.modifyUrl Route.Home
                    else
                        Cmd.none
            in
            ( { model | session = { session | user = user } }, cmd )

        ( ToolbarMsg msg_, _ ) ->
            let
                ( toolbar, effects ) =
                    Views.Toolbar.update ToolbarMsg msg_ model.toolbar

                -- Pass message along to the drawer.
                ( newModel, drawerEffects ) =
                    case Debug.log "drawerMsg.msg_" msg_ of
                        Views.Toolbar.OpenDrawer ->
                            update (DrawerMsg Views.Drawer.OpenDrawer) model

                        Views.Toolbar.CloseDrawer ->
                            update (DrawerMsg Views.Drawer.CloseDrawer) model

                        _ ->
                            ( model, Cmd.none )
            in
            ( { newModel | toolbar = toolbar }
            , Cmd.batch [ effects, drawerEffects ]
            )

        ( DrawerMsg msg_, _ ) ->
            let
                ( drawer, effects ) =
                    Views.Drawer.update DrawerMsg msg_ model.drawer
            in
            ( { model | drawer = drawer }, effects )

        ( HomeMsg msg_, Home home_ ) ->
            let
                ( home, effects ) =
                    Pages.Home.update HomeMsg msg_ model.session home_
            in
            ( { model | pageState = Loaded (Home home) }, effects )

        ( HomeLoaded (Ok home), _ ) ->
            ( { model | pageState = Loaded (Home home) }, Cmd.none )

        ( HomeLoaded (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( LoginMsg msg_, _ ) ->
            let
                ( login, effects ) =
                    Pages.Login.update LoginMsg msg_ model.login
            in
            case msg_ of
                -- Update the session. There's gotta be a better way.
                Pages.Login.LoginCompleted (Ok user) ->
                    ( { model
                        | login = login
                        , session = { session | user = Just user }
                      }
                    , effects
                    )

                _ ->
                    ( { model | login = login }, effects )

        ( OtherMsg msg_, _ ) ->
            let
                ( other, effects ) =
                    Pages.Other.update OtherMsg msg_ model.other
            in
            ( { model | other = other }, effects )

        ( OtherLoaded (Ok home), _ ) ->
            ( { model | error = "", pageState = Loaded Other }, Cmd.none )

        ( OtherLoaded (Err error), _ ) ->
            ( { model | error = error, pageState = Loaded Other }, Cmd.none )

        ( Click, _ ) ->
            ( model, Cmd.none )

        ( ProfileMsg msg_, Profile username model_ ) ->
            let
                ( profile, effects ) =
                    Pages.Profile.update ProfileMsg msg_ model.session model_
            in
            ( { model | pageState = Loaded (Profile username profile) }, effects )

        ( ProfileLoaded username (Ok profile), _ ) ->
            ( { model
                | pageState = Loaded (Profile username profile)
              }
            , Cmd.none
            )

        ( ProfileLoaded username (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( ArticleLoaded (Ok article), _ ) ->
            ( { model | pageState = Loaded Article }, Cmd.none )

        ( ArticleLoaded (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( EditArticleLoaded slug (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Editor (Just slug)) }, Cmd.none )

        ( EditArticleLoaded slug (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            ( model, Cmd.none )

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            ( model, Cmd.none )


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ( { model
                | pageState = TransitioningFrom (getPage model.pageState)
              }
            , Task.attempt toMsg task
            )

        errored =
            pageErrored model
    in
    case Debug.log "Main.setRoute maybeRoute" maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Home ->
            transition HomeLoaded (Pages.Home.init model.session)

        Just Route.NewArticle ->
            case model.session.user of
                Just user ->
                    -- ({ model | pageState = Loaded (Editor Nothing Editor.initNew) }, Cmd.none)
                    -- slug and data need to be saved to Main.Model
                    ( { model | pageState = Loaded (Editor Nothing) }, Cmd.none )

                Nothing ->
                    errored Views.Page.NewArticle "You must be signed in to post an article."

        Just (Route.EditArticle slug) ->
            case model.session.user of
                Just user ->
                    transition (EditArticleLoaded slug) (Editor.initEdit model.session slug)

                Nothing ->
                    errored Views.Page.Other "You must be signed in to edit an article."

        Just (Route.Profile username) ->
            transition (ProfileLoaded username) (Pages.Profile.init model.session username)

        Just (Route.Article slug) ->
            transition ArticleLoaded (Article.init model.session slug)

        Just Route.Root ->
            ( model, Route.modifyUrl Route.Home )

        Just Route.Login ->
            ( { model | pageState = Loaded Login }, Cmd.none )

        Just Route.Other ->
            transition OtherLoaded
                (Task.map
                    (\_ -> model.other)
                    (Process.sleep (Time.second * 2))
                )

        Just Route.Register ->
            -- { model | pageState = Loaded (Register Register.initialModel) } => Cmd.none
            ( { model | pageState = Loaded Register }, Cmd.none )


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    ( { model | pageState = Loaded (Errored error) }, Cmd.none )


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page



-- VIEW


view : Model -> Html Msg
view model =
    case Debug.log "Main.view model.pageState" model.pageState of
        Loaded page ->
            viewPage model False page

        TransitioningFrom page ->
            viewPage model True page


viewPage : Model -> Bool -> Page -> Html Msg
viewPage model isLoading page =
    let
        user =
            model.session.user

        -- yuck
        activePage =
            case page of
                Home home ->
                    Views.Page.Home

                Login ->
                    Views.Page.Login

                _ ->
                    Views.Page.Other

        pageContext =
            { setRoute = SetRoute
            , setUser = SetUser
            , isLoading = isLoading
            , session = model.session
            , user = user
            , body =
                \title nodes ->
                    let
                        drawer =
                            model.drawer

                        toolbar =
                            model.toolbar

                        viewContext =
                            View.Context
                                activePage
                                isLoading
                                SetRoute
                                SetUser
                                user
                                title

                        drawerView =
                            Views.Drawer.view
                                DrawerMsg
                                viewContext
                                drawer

                        toolbarView =
                            Views.Toolbar.view
                                ToolbarMsg
                                viewContext
                                toolbar

                        dumbSpacer =
                            styled Html.div
                                [ css "height" "36px"
                                ]
                                []

                        nodes_ =
                            styled Html.div
                                [ css "padding" "24px"
                                ]
                                nodes
                    in
                    styled Html.div
                        [ Typography.typography
                        ]
                        [ drawerView
                        , toolbarView
                        , dumbSpacer
                        , nodes_
                        ]
            }

        viewNotFound =
            Html.div []
                [ Button.view Mdc
                    "my-button"
                    model.mdc
                    [ Button.ripple
                    , Options.onClick Click
                    ]
                    [ text "Click me!" ]
                ]
    in
    case Debug.log "Main.getPage model.pageState" getPage model.pageState of
        Home home ->
            Pages.Home.view HomeMsg pageContext home

        Login ->
            Pages.Login.view LoginMsg pageContext model.login

        Profile username profile ->
            Pages.Profile.view ProfileMsg pageContext profile

        Other ->
            Pages.Other.view OtherMsg pageContext model.other

        _ ->
            viewNotFound



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdc model
        , Sub.map SetUser sessionChange
        ]


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)
