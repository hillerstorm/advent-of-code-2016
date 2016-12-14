module Day2.Part2 exposing (main)

import Html exposing (..)
import Day2.Input exposing (rawInput, parsedInput, Direction(..))


solve : Key -> String -> List (List Direction) -> String
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


type Key
    = N Int
    | A
    | B
    | C
    | D


solveRow : List Direction -> Key -> String -> ( Key, String )
solveRow list key code =
    case list of
        [] ->
            let
                strKey =
                    case key of
                        N i ->
                            toString i

                        _ ->
                            toString key
            in
                ( key, code ++ strKey )

        dir :: rest ->
            let
                nextKey =
                    move dir key
            in
                solveRow rest nextKey code


move : Direction -> Key -> Key
move direction key =
    case key of
        N 1 ->
            case direction of
                Down ->
                    N 3

                _ ->
                    key

        N 2 ->
            case direction of
                Down ->
                    N 6

                Right ->
                    N 3

                _ ->
                    key

        N 3 ->
            case direction of
                Up ->
                    N 1

                Down ->
                    N 7

                Left ->
                    N 2

                Right ->
                    N 4

        N 4 ->
            case direction of
                Down ->
                    N 8

                Left ->
                    N 3

                _ ->
                    key

        N 5 ->
            case direction of
                Right ->
                    N 6

                _ ->
                    key

        N 6 ->
            case direction of
                Up ->
                    N 2

                Down ->
                    A

                Left ->
                    N 5

                Right ->
                    N 7

        N 7 ->
            case direction of
                Up ->
                    N 3

                Down ->
                    B

                Left ->
                    N 6

                Right ->
                    N 8

        N 8 ->
            case direction of
                Up ->
                    N 4

                Down ->
                    C

                Left ->
                    N 7

                Right ->
                    N 9

        N 9 ->
            case direction of
                Left ->
                    N 8

                _ ->
                    key

        A ->
            case direction of
                Up ->
                    N 6

                Right ->
                    B

                _ ->
                    key

        B ->
            case direction of
                Up ->
                    N 7

                Down ->
                    D

                Left ->
                    A

                Right ->
                    C

        C ->
            case direction of
                Up ->
                    N 8

                Left ->
                    B

                _ ->
                    key

        D ->
            case direction of
                Up ->
                    B

                _ ->
                    key

        _ ->
            key


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (solve (N 5) "" parsedInput |> toString)) ]
        ]
