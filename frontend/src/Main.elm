module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Page.Home
import Page.NotFound
import Url exposing (Url)
import Url.Parser as Parser


type alias Model =
    { key : Nav.Key
    , route : Route
    }


type Route
    = Home
    | NotFound


routeFromUrl : Url -> Route
routeFromUrl url =
    Parser.parse (Parser.map Home Parser.top) url
        |> Maybe.withDefault NotFound


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url


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
    ( { key = key, route = routeFromUrl url }, Cmd.none )


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
            ( { model | route = routeFromUrl url }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Home ->
            Page.Home.view

        NotFound ->
            Page.NotFound.view
