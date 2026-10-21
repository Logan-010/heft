module Main exposing (main)

import Api.Auth
import Api.Util
import Browser
import Browser.Navigation as Nav
import Html
import Http
import Page.Home
import Page.Login
import Page.NotFound
import Url exposing (Url)
import Url.Parser as Parser


type alias Model =
    { key : Nav.Key
    , route : Route
    , loggedIn : Maybe Bool
    , error : Maybe String
    , loginModel : Page.Login.Model
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
    ( { key = key, route = routeFromUrl url, loggedIn = Nothing, error = Nothing, loginModel = Page.Login.init () }
    , Cmd.map AuthMsg (Api.Auth.isLoggedIn (Url.toString { url | path = "", query = Nothing, fragment = Nothing }))
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
            in
            ( updatedModel, redirectIfLoggedOut updatedModel )

        AuthMsg (Api.Auth.IsLoggedIn result) ->
            case result of
                Ok _ ->
                    ( { model | loggedIn = Just True, error = Nothing }, Cmd.none )

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
            in
            ( { model | loginModel = loginModel }, Cmd.map LoginMsg loginCmd )


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
    case model.error of
        Just error ->
            { document | body = document.body ++ [ Html.p [] [ Html.text error ] ] }

        Nothing ->
            document


viewRoute : Model -> Browser.Document Msg
viewRoute model =
    case model.route of
        Home ->
            Page.Home.view

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
