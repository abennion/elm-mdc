module Pages.Profile exposing (Model, Msg(Mdc), defaultModel, init, update, view)

{-| Viewing a user's profile.
-}

import Data.Profile exposing (Profile, defaultProfile)
import Data.Session exposing (Session)
import Data.User as User exposing (Username, stringToUsername)
import Data.UserPhoto as UserPhoto exposing (UserPhoto, maybeStringToUserPhoto)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Material
import Pages.Errored exposing (PageLoadError, pageLoadError)
import Request.Article exposing (ListConfig, defaultListConfig)
import Request.Profile
import SelectList exposing (SelectList)
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Article.Feed as Feed exposing (FeedSource, authorFeed, defaultModel, favoritedFeed)
import Views.Errors as Errors
import Views.Page as Page exposing (ActivePage, Context)
import Views.User.Follow as Follow


-- MODEL --


type alias Model m =
    { mdc : Material.Model m
    , errors : List String
    , profile : Profile
    , feed : Feed.Model
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , errors = []
    , profile = Data.Profile.defaultProfile
    , feed = Feed.defaultModel
    }


init : Session -> Username -> Task PageLoadError (Model m)
init session username =
    let
        config : ListConfig
        config =
            { defaultListConfig | limit = 5, author = Just username }

        maybeAuthToken =
            session.user
                |> Maybe.map .token

        loadProfile =
            Request.Profile.get username maybeAuthToken
                |> Http.toTask

        loadFeedSources =
            Feed.init session (defaultFeedSources username)

        handleLoadError _ =
            "Profile is currently unavailable."
                |> pageLoadError (Page.Profile username)
    in
    Task.map2 (Model Material.defaultModel []) loadProfile loadFeedSources
        |> Task.mapError handleLoadError


view : (Msg m -> m) -> Context m -> Model m -> Html m
view lift context model =
    let
        profile =
            model.profile

        session =
            context.session

        isMyProfile =
            session.user
                |> Maybe.map (\{ username } -> username == profile.username)
                |> Maybe.withDefault False

        _ =
            Debug.log "Profile.view" session
    in
    context.body "Home"
        [ div [ class "profile-page" ]
            [ -- Errors.view DismissErrors model.errors
              div [ class "user-info" ]
                [ div [ class "container" ]
                    [ div [ class "row" ]
                        [ viewProfileInfo lift isMyProfile profile ]
                    ]
                ]
            , div [ class "container" ]
                [ div [ class "row" ] [ viewFeed lift model.feed ] ]
            ]
        ]


viewProfileInfo : (Msg m -> m) -> Bool -> Profile -> Html m
viewProfileInfo lift isMyProfile profile =
    let
        _ =
            Debug.log "Feed.viewProfileInfo" ""
    in
    div [ class "col-xs-12 col-md-10 offset-md-1" ]
        [ img [ class "user-img", UserPhoto.src profile.image ] []
        , h4 [] [ User.usernameToHtml profile.username ]
        , p [] [ text (Maybe.withDefault "" profile.bio) ]
        , viewIf (not isMyProfile) (followButton lift profile)
        ]


viewFeed : (Msg m -> m) -> Feed.Model -> Html m
viewFeed lift feed =
    let
        _ =
            Debug.log "Profile.viewFeed" ""
    in
    div [ class "col-xs-12 col-md-10 offset-md-1" ] <|
        div [ class "articles-toggle" ]
            [ Feed.viewFeedSources feed |> Html.map (lift << FeedMsg) ]
            :: (Feed.viewArticles feed |> List.map (Html.map (lift << FeedMsg)))



-- UPDATE --


type Msg m
    = Mdc (Material.Msg m)
    | DismissErrors
    | ToggleFollow
    | FollowCompleted (Result Http.Error Profile)
    | FeedMsg Feed.Msg


update : (Msg m -> m) -> Msg m -> Session -> Model m -> ( Model m, Cmd m )
update lift msg session model =
    let
        profile =
            model.profile
    in
    case Debug.log "Profile.update" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        DismissErrors ->
            { model | errors = [] } => Cmd.none

        ToggleFollow ->
            case session.user of
                Nothing ->
                    { model | errors = model.errors ++ [ "You are currently signed out. You must be signed in to follow people." ] }
                        => Cmd.none

                Just user ->
                    user.token
                        |> Request.Profile.toggleFollow
                            profile.username
                            profile.following
                        |> Http.send (lift << FollowCompleted)
                        |> pair model

        FollowCompleted (Ok newProfile) ->
            { model | profile = newProfile } => Cmd.none

        FollowCompleted (Err error) ->
            model => Cmd.none

        FeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update session subMsg model.feed
            in
            { model | feed = newFeed } => Cmd.map (lift << FeedMsg) subCmd


followButton : (Msg m -> m) -> Profile -> Html m
followButton lift =
    Follow.button (\_ -> lift ToggleFollow)



-- INTERNAL --


defaultFeedSources : Username -> SelectList FeedSource
defaultFeedSources username =
    SelectList.fromLists [] (authorFeed username) [ favoritedFeed username ]
