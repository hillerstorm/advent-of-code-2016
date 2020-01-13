module Day03.Part1 exposing (main)

import Day03.Input exposing (parsedInput)
import Html exposing (Html, div, text)


isValid : List Int -> Bool
isValid list =
    case list of
        [ a, b, c ] ->
            a + b > c && a + c > b && b + c > a

        _ ->
            False


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ (String.fromInt <| List.length <| List.filter isValid parsedInput))
            ]
        ]
