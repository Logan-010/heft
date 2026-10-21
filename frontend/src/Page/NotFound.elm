module Page.NotFound exposing (view)

import Browser
import Html exposing (a, div, h1, text)
import Html.Attributes exposing (class, href)
import Style


view : Browser.Document msg
view =
    { title = "404 - Heft"
    , body =
        [ div [ class "space-y-6" ]
            [ h1 [ class (Style.heading ++ " text-lg sm:text-xl") ] [ text "404 - Page not found" ]
            , a [ href "/", class "inline-block py-2 text-accent underline decoration-line underline-offset-4 hover:decoration-accent focus-visible:outline-2 focus-visible:outline-offset-4 focus-visible:outline-accent" ] [ text "Back to home" ]
            ]
        ]
    }
