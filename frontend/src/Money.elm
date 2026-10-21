module Money exposing (format, parse)

{-| Money is stored and sent to the API as integer hundredths.
-}


parse : String -> Maybe Int
parse amount =
    let
        compact =
            String.filter (\character -> not (List.member character [ ',', ' ', '\u{00A0}', '\u{202F}', '\t', '\n', '\u{000D}', '_', '\'', '$', '€', '£', '¥', '₹', '₩' ])) amount
                |> String.replace "−" "-"

        normalized =
            if String.startsWith "(" compact && String.endsWith ")" compact then
                let
                    enclosed =
                        String.dropLeft 1 compact |> String.dropRight 1
                in
                if String.startsWith "-" enclosed || String.startsWith "+" enclosed then
                    ""

                else
                    "-" ++ enclosed

            else
                compact

        negative =
            String.startsWith "-" normalized

        unsigned =
            if negative || String.startsWith "+" normalized then
                String.dropLeft 1 normalized

            else
                normalized

        digits value =
            String.all (\character -> character >= '0' && character <= '9') value

        hundredths whole fraction =
            if (String.isEmpty whole && String.isEmpty fraction) || not (digits whole && digits fraction) || String.length fraction > 2 then
                Nothing

            else
                String.toInt
                    ((if String.isEmpty whole then
                        "0"

                      else
                        whole
                     )
                        ++ String.padRight 2 '0' fraction
                    )
                    |> Maybe.map
                        (\value ->
                            if negative then
                                -value

                            else
                                value
                        )
    in
    case String.split "." unsigned of
        [ whole ] ->
            hundredths whole ""

        [ whole, fraction ] ->
            hundredths whole fraction

        _ ->
            Nothing


format : Int -> String
format amount =
    let
        digits =
            String.fromInt (abs amount) |> String.padLeft 3 '0'

        sign =
            if amount < 0 then
                "-"

            else
                ""
    in
    sign ++ String.dropRight 2 digits ++ "." ++ String.right 2 digits
