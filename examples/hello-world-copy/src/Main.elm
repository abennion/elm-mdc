module Main exposing (..)

-- import Pages.Errored exposing (PageLoadError)

import Data.Session exposing (Session)
import Data.User as User exposing (User, Username)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, styled)
import Material.Typography as Typography
import Navigation exposing (Location)
import Pages.Home exposing (Model, Msg(Mdc), defaultModel, update, view)
import Pages.Login exposing (Model, Msg(..), defaultModel, update, view)
import Pages.Other exposing (Model, Msg(Mdc), defaultModel, update, view)
import Pages.Page as Page exposing (Context, Page)
import Ports exposing (..)
import Process
import Route exposing (Route)
import Task
import Time
import Views.Drawer exposing (Model, Msg(..), defaultModel, update, view)
import Views.Toolbar exposing (Model, Msg(..), defaultModel, update, view)
import Views.View as View exposing (Context)


{-| TODO

  - Pass messages from components without exposing Msg values?
    Use an external message instead?
  - PageLoadError
  - Can we do a persistent drawer with the top app bar?
  - How to do proper formatting of main body under top app bar?

-}



-- MODEL


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
    , home : Pages.Home.Model Msg
    , login : Pages.Login.Model Msg
    , other : Pages.Other.Model Msg
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , session = { user = Nothing }
    , pageState = Loaded Page.Home
    , error = "nothing"
    , toolbar = Views.Toolbar.defaultModel
    , drawer = Views.Drawer.defaultModel
    , home = Pages.Home.defaultModel
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
    | HomeLoaded (Result ErrorMsg (Pages.Home.Model Msg))
    | LoginMsg (Pages.Login.Msg Msg)
    | OtherMsg (Pages.Other.Msg Msg)
    | OtherLoaded (Result ErrorMsg (Pages.Other.Model Msg))
    | Click



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
        session =
            model.session
    in
    case Debug.log "Main.update msg" msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetRoute route ->
            setRoute route model

        SetUser user ->
            let
                cmd =
                    if session.user /= Nothing && user == Nothing then
                        Route.modifyUrl Route.Home
                    else
                        Cmd.none
            in
            ( { model | session = { session | user = user } }, cmd )

        ToolbarMsg msg_ ->
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

        DrawerMsg msg_ ->
            let
                ( drawer, effects ) =
                    Views.Drawer.update DrawerMsg msg_ model.drawer
            in
            ( { model | drawer = drawer }, effects )

        HomeMsg msg_ ->
            let
                ( home, effects ) =
                    Pages.Home.update HomeMsg msg_ model.home
            in
            ( { model | home = home }, effects )

        HomeLoaded (Ok home) ->
            ( { model | error = "", pageState = Loaded Page.Home }, Cmd.none )

        HomeLoaded (Err error) ->
            ( { model | error = error, pageState = Loaded Page.Home }, Cmd.none )

        LoginMsg msg_ ->
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

        OtherMsg msg_ ->
            let
                ( other, effects ) =
                    Pages.Other.update OtherMsg msg_ model.other
            in
            ( { model | other = other }, effects )

        OtherLoaded (Ok home) ->
            ( { model | error = "", pageState = Loaded Page.Other }, Cmd.none )

        OtherLoaded (Err error) ->
            ( { model | error = error, pageState = Loaded Page.Other }, Cmd.none )

        Click ->
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
    in
    case Debug.log "Main.setRoute maybeRoute" maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Home ->
            transition HomeLoaded
                (Task.map
                    (\_ -> model.home)
                    (Process.sleep (Time.second * 2))
                )

        Just Route.Root ->
            ( { model | pageState = Loaded Page.Home }, Cmd.none )

        Just Route.Login ->
            ( { model | pageState = Loaded Page.Login }, Cmd.none )

        Just Route.Other ->
            transition OtherLoaded
                (Task.map
                    (\_ -> model.other)
                    (Process.sleep (Time.second * 2))
                )


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

        pageContext =
            { setRoute = SetRoute
            , setUser = SetUser
            , isLoading = isLoading
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
                                page
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
        Page.Home ->
            Pages.Home.view HomeMsg pageContext model.home

        Page.Other ->
            Pages.Other.view OtherMsg pageContext model.other

        Page.Login ->
            Pages.Login.view LoginMsg pageContext model.login

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
