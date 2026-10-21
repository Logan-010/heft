module Page.Login exposing (Model, Msg, init, update, view)

import Api.Auth
import Browser
import Html exposing (div)


type alias Model =
    {}


type Msg
    = LoggedIn Api.Auth.AuthMsg


init : () -> Model
init _ =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Login - Heft", body = [ div [] [] ] }
