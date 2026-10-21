module Page.Home exposing (view)

import Browser
import Html exposing (div, h1, p, text)


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
