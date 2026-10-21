module Page.Home exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (div, h1, p, text)


type Msg
    = Todo


type alias Model =
    { url : String
    }


init : String -> Model
init u =
    { url = u }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Browser.Document msg
view =
    { title = "Heft"
    , body =
        [ div []
            [ h1 [] [ text "Heft" ]
            , p [] [ text "Personal financial tracking" ]
            ]
        ]
    }
