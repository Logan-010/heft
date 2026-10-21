port module Main exposing (main)

import Api.Auth
import Api.Util
import Browser
import Html
import Html.Attributes exposing (attribute, class)
import Http
import Page.Home
import Page.Login
import Page.NotFound
import Style


type alias Model =
    { route : Route
    , loggedIn : Maybe Bool
    , error : Maybe String
    , loginModel : Page.Login.Model
    , homeModel : Page.Home.Model
    }


type Route
    = Home
    | Login
    | NotFound


port replacePath : String -> Cmd msg


port pathChanged : (String -> msg) -> Sub msg


routeFromPath : String -> Route
routeFromPath path =
    case List.filter (not << String.isEmpty) (String.split "/" path) of
        [] ->
            Home

        [ "login" ] ->
            Login

        _ ->
            NotFound


type Msg
    = PathChanged String
    | AuthMsg Api.Auth.AuthMsg
    | HomeMsg Page.Home.Msg
    | LoginMsg Page.Login.Msg


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> pathChanged PathChanged
        }


init : String -> ( Model, Cmd Msg )
init path =
    ( { route = routeFromPath path, loggedIn = Nothing, error = Nothing, loginModel = Page.Login.init, homeModel = Page.Home.init }
    , Cmd.map AuthMsg Api.Auth.isLoggedIn
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PathChanged path ->
            let
                updatedModel =
                    { model | route = routeFromPath path }

                ( loadedModel, homeCmd ) =
                    loadHome updatedModel
            in
            ( loadedModel, Cmd.batch [ redirectIfLoggedOut loadedModel, homeCmd ] )

        AuthMsg (Api.Auth.IsLoggedIn result) ->
            case result of
                Ok _ ->
                    loadHome { model | loggedIn = Just True, error = Nothing }

                Err (Http.BadStatus 401) ->
                    let
                        updatedModel =
                            { model | loggedIn = Just False, error = Nothing }
                    in
                    ( updatedModel, redirectIfLoggedOut updatedModel )

                Err error ->
                    ( { model | error = Just (Api.Util.httpErrorToString error) }, Cmd.none )

        AuthMsg _ ->
            ( model, Cmd.none )

        LoginMsg loginMsg ->
            let
                ( loginModel, loginCmd ) =
                    Page.Login.update loginMsg model.loginModel

                loggedIn =
                    case loginMsg of
                        Page.Login.LoggedIn (Api.Auth.LogIn (Ok _)) ->
                            Just True

                        Page.Login.Registered (Api.Auth.Register (Ok _)) ->
                            Just True

                        Page.Login.LoggedOut (Api.Auth.LogOut (Ok _)) ->
                            Just False

                        _ ->
                            model.loggedIn
            in
            ( { model | loginModel = loginModel, loggedIn = loggedIn }
            , Cmd.batch
                [ Cmd.map LoginMsg loginCmd
                , if loggedIn == Just True && model.loggedIn /= Just True then
                    replacePath "/"

                  else
                    Cmd.none
                ]
            )

        HomeMsg homeMsg ->
            let
                ( homeModel, homeCmd ) =
                    Page.Home.update homeMsg model.homeModel
            in
            ( { model | homeModel = homeModel }, Cmd.map HomeMsg homeCmd )


loadHome : Model -> ( Model, Cmd Msg )
loadHome model =
    if model.route == Home && model.loggedIn == Just True then
        let
            ( homeModel, homeCmd ) =
                Page.Home.update Page.Home.RefreshAccounts model.homeModel
        in
        ( { model | homeModel = homeModel }, Cmd.map HomeMsg homeCmd )

    else
        ( model, Cmd.none )


redirectIfLoggedOut : Model -> Cmd Msg
redirectIfLoggedOut model =
    if model.loggedIn == Just False && model.route /= Login then
        replacePath "/login"

    else
        Cmd.none


view : Model -> Browser.Document Msg
view model =
    let
        document =
            viewRoute model
    in
    { document
        | body =
            [ Html.main_
                [ class
                    (if model.route == Home then
                        "min-h-svh p-4 sm:p-8"

                     else
                        "grid min-h-svh place-items-center px-4 py-8 sm:px-8 sm:py-16"
                    )
                ]
                [ Html.div
                    [ class
                        (if model.route == Home then
                            "mx-auto w-full min-w-0 max-w-screen-2xl"

                         else
                            "w-full max-w-lg min-w-0 border border-line bg-surface p-6 sm:p-10"
                        )
                    ]
                    (document.body
                        ++ (case model.error of
                                Just error ->
                                    [ Html.p
                                        [ class (Style.message ++ " mt-6")
                                        , attribute "role" "alert"
                                        ]
                                        [ Html.text error ]
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                ]
            ]
    }


viewRoute : Model -> Browser.Document Msg
viewRoute model =
    case model.route of
        Home ->
            let
                document =
                    Page.Home.view model.homeModel
            in
            { title = document.title
            , body = List.map (Html.map HomeMsg) document.body
            }

        Login ->
            let
                document =
                    Page.Login.view model.loginModel
            in
            { title = document.title
            , body = List.map (Html.map LoginMsg) document.body
            }

        NotFound ->
            Page.NotFound.view
