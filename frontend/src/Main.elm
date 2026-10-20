module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


main : Program () Model Msg
main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Heft" ]
        , p [] [ text "Personal financial tracking" ]
        , button [ onClick Decrement ] [ text "-" ]
        , text (" " ++ String.fromInt model ++ " ")
        , button [ onClick Increment ] [ text "+" ]
        ]
