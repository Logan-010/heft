module Page.NotFound exposing (view)

import Browser
import Html exposing (a, div, h1, text)
import Html.Attributes exposing (href)


view : Browser.Document msg
view =
    { title = "404 - Heft"
    , body =
        [ div []
            [ h1 [] [ text "404 - Page not found" ]
            , a [ href "/" ] [ text "Back to home" ]
            ]
        ]
    }
