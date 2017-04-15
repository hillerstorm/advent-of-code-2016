module Day2.Part1 exposing (main)

import Html exposing (..)
import Tuple exposing (first)
import Day2.Input exposing (rawInput, parsedInput, Direction(..))


solve : List (List Direction) -> ( String, Int ) -> String
solve list =
    case list of
        [] ->
            first

        row :: rest ->
            solve rest << solveRow row


solveRow : List Direction -> ( String, Int ) -> ( String, Int )
solveRow list ( code, key ) =
    case list of
        [] ->
            ( code ++ (toString key), key )

        dir :: rest ->
            solveRow rest ( code, move dir key )


move : Direction -> Int -> Int
move direction key =
    if direction == Up && key > 3 then
        key - 3
    else if direction == Down && key < 7 then
        key + 3
    else if direction == Left && (key + 2) % 3 > 0 then
        key - 1
    else if direction == Right && key % 3 > 0 then
        key + 1
    else
        key


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (solve parsedInput ( "", 5 ))) ]
        ]
