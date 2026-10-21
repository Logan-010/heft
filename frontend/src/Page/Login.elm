module Page.Login exposing (Model, Msg(..), init, update, view)

import Api.Auth
import Api.Util
import Browser
import Html exposing (button, div, input, text)
import Html.Attributes exposing (disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { username : String
    , password : String
    , secret : String
    , register : Bool
    , message : Maybe String
    , loading : Bool
    , url : String
    }


type Msg
    = LoggedIn Api.Auth.AuthMsg
    | Registered Api.Auth.AuthMsg
    | LoggedOut Api.Auth.AuthMsg
    | UpdateUsername String
    | UpdatePassword String
    | UpdateSecret String
    | Send
    | LogOut
    | Toggle


init : String -> Model
init u =
    { username = "", password = "", secret = "", register = False, message = Nothing, loading = False, url = u }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoggedIn (Api.Auth.LogIn res) ->
            case res of
                Ok s ->
                    ( { model | message = Just s, loading = False }, Cmd.none )

                Err e ->
                    ( { model | message = Just (Api.Util.httpErrorToString e), loading = False }, Cmd.none )

        LoggedIn _ ->
            ( model, Cmd.none )

        Registered (Api.Auth.Register res) ->
            case res of
                Ok s ->
                    ( { model | message = Just s, loading = False }, Cmd.none )

                Err e ->
                    ( { model | message = Just (Api.Util.httpErrorToString e), loading = False }, Cmd.none )

        Registered _ ->
            ( model, Cmd.none )

        LoggedOut (Api.Auth.LogOut res) ->
            case res of
                Ok s ->
                    ( { model | username = "", password = "", secret = "", message = Just s, loading = False }, Cmd.none )

                Err e ->
                    ( { model | message = Just (Api.Util.httpErrorToString e), loading = False }, Cmd.none )

        LoggedOut _ ->
            ( model, Cmd.none )

        UpdateUsername u ->
            ( { model | username = u }, Cmd.none )

        UpdatePassword p ->
            ( { model | password = p }, Cmd.none )

        UpdateSecret s ->
            ( { model | secret = s }, Cmd.none )

        Send ->
            ( { model | loading = True }
            , if model.register then
                Cmd.map Registered (Api.Auth.register model.url model.username model.password model.secret)

              else
                Cmd.map LoggedIn (Api.Auth.logIn model.url model.username model.password)
            )

        LogOut ->
            ( { model | loading = True, message = Nothing }
            , Cmd.map LoggedOut (Api.Auth.logOut model.url)
            )

        Toggle ->
            ( { model | register = not model.register }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Login - Heft"
    , body =
        [ case model.message of
            Just e ->
                text e

            Nothing ->
                text ""
        , div []
            [ input
                [ type_ "text"
                , placeholder "Enter username..."
                , onInput UpdateUsername
                , value model.username
                ]
                []
            , input
                [ type_ "password"
                , placeholder "Enter password..."
                , onInput UpdatePassword
                , value model.password
                ]
                []
            , if model.register then
                input [ type_ "text", placeholder "Enter secret...", onInput UpdateSecret, value model.secret ] []

              else
                text ""
            , button
                [ onClick Send, disabled model.loading ]
                [ text "Send" ]
            , button
                [ onClick Toggle, disabled model.loading ]
                [ text
                    (if model.register then
                        "Login"

                     else
                        "Register"
                    )
                ]
            , button
                [ onClick LogOut, disabled model.loading ]
                [ text "Log out" ]
            ]
        ]
    }
