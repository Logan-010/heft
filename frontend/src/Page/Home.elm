module Page.Home exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (div, h1, p, text)
import Html.Attributes exposing (class)
import Style


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
        [ div [ class "space-y-4" ]
            [ h1 [ class (Style.heading ++ " text-xl sm:text-2xl") ] [ text "Heft" ]
            , p [ class "text-muted" ] [ text "Personal financial tracking" ]
            ]
        ]
    }
