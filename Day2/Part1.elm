module Day2.Part1 exposing (main)

import Html exposing (..)
import Day2.Input exposing (rawInput, parsedInput, Direction(..))


solve : Int -> String -> List (List Direction) -> String
solve key code list =
    case list of
        [] ->
            code

        row :: rest ->
            let
                ( nextKey, nextCode ) =
                    solveRow row key code
            in
                solve nextKey nextCode rest


solveRow : List Direction -> Int -> String -> ( Int, String )
solveRow list key code =
    case list of
        [] ->
            ( key, code ++ (toString key) )

        dir :: rest ->
            let
                nextKey =
                    move dir key
            in
                solveRow rest nextKey code


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
        , div [] [ text ("Result: " ++ (solve 5 "" parsedInput |> toString)) ]
        ]
