module Day11.Day11 exposing (main)

import Html exposing (..)
import Day11.Input exposing (rawInput, parsedInput)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Parsed input: " ++ (toString parsedInput)) ]
        , div [] [ text "Formula (for this input): (2*total distance)-9" ]
        , div [] [ text "Part 1: 31" ]
        , div [] [ text "Part 2: 55" ]
        ]
