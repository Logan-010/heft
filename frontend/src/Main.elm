module Main exposing (main)

import Api.Auth
import Api.Util
import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes exposing (attribute, class)
import Http
import Page.Home
import Page.Login
import Page.NotFound
import Style
import Url exposing (Url)
import Url.Parser as Parser


type alias Model =
    { key : Nav.Key
    , route : Route
    , loggedIn : Maybe Bool
    , error : Maybe String
    , loginModel : Page.Login.Model
    , homeModel : Page.Home.Model
    }


type Route
    = Home
    | Login
    | NotFound


routeFromUrl : Url -> Route
routeFromUrl url =
    Parser.parse
        (Parser.oneOf
            [ Parser.map Home Parser.top
            , Parser.map Login (Parser.s "login")
            ]
        )
        url
        |> Maybe.withDefault NotFound


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | AuthMsg Api.Auth.AuthMsg
    | HomeMsg Page.Home.Msg
    | LoginMsg Page.Login.Msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        u =
            Url.toString { url | path = "", query = Nothing, fragment = Nothing }
    in
    ( { key = key, route = routeFromUrl url, loggedIn = Nothing, error = Nothing, loginModel = Page.Login.init u, homeModel = Page.Home.init u }
    , Cmd.map AuthMsg (Api.Auth.isLoggedIn u)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            let
                updatedModel =
                    { model | route = routeFromUrl url }

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
                    Nav.replaceUrl model.key "/"

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
        Nav.replaceUrl model.key "/login"

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
