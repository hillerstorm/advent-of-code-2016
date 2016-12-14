module Day3.Part1 exposing (main)

import Html exposing (..)
import Day3.Input exposing (rawInput, parsedInput)


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
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| List.length <| List.filter isValid parsedInput)) ]
        ]
