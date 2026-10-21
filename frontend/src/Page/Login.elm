module Page.Login exposing (Model, Msg(..), init, update, view)

import Api.Auth
import Api.Util
import Browser
import Html exposing (button, div, input, text)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Style


type alias Model =
    { username : String
    , password : String
    , secret : String
    , register : Bool
    , message : Maybe String
    , loading : Bool
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


init : Model
init =
    { username = "", password = "", secret = "", register = False, message = Nothing, loading = False }


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
                Cmd.map Registered (Api.Auth.register model.username model.password model.secret)

              else
                Cmd.map LoggedIn (Api.Auth.logIn model.username model.password)
            )

        LogOut ->
            ( { model | loading = True, message = Nothing }
            , Cmd.map LoggedOut Api.Auth.logOut
            )

        Toggle ->
            ( { model | register = not model.register }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Login - Heft"
    , body =
        [ case model.message of
            Just e ->
                div [ class (Style.message ++ " mb-6"), attribute "role" "status", attribute "aria-live" "polite" ] [ text e ]

            Nothing ->
                text ""
        , div
            [ class "space-y-3"
            , attribute "aria-busy"
                (if model.loading then
                    "true"

                 else
                    "false"
                )
            ]
            [ input
                [ type_ "text"
                , class Style.input
                , attribute "aria-label" "Username"
                , placeholder "Enter username..."
                , onInput UpdateUsername
                , value model.username
                ]
                []
            , input
                [ type_ "password"
                , class Style.input
                , attribute "aria-label" "Password"
                , placeholder "Enter password..."
                , onInput UpdatePassword
                , value model.password
                ]
                []
            , if model.register then
                input [ type_ "text", class Style.input, attribute "aria-label" "Secret", placeholder "Enter secret...", onInput UpdateSecret, value model.secret ] []

              else
                text ""
            , div [ class "grid grid-cols-1 gap-3 border-t border-line pt-5 sm:grid-cols-3" ]
                [ button
                    [ onClick Send, disabled model.loading, class Style.buttonPrimary ]
                    [ text "Send" ]
                , button
                    [ onClick Toggle, disabled model.loading, class Style.buttonSecondary ]
                    [ text
                        (if model.register then
                            "Login"

                         else
                            "Register"
                        )
                    ]
                , button
                    [ onClick LogOut, disabled model.loading, class Style.buttonQuiet ]
                    [ text "Log out" ]
                ]
            ]
        ]
    }
