module Day11.Day11 exposing (main)

import Day11.Input exposing (parsedInput, rawInput)
import Html exposing (Html, div, text)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text "Formula (for this input): (2*total distance)-9" ]
        , div [] [ text "Part 1: 31" ]
        , div [] [ text "Part 2: 55" ]
        ]
