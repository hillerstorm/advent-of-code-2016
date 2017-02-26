module Day18.Day18 exposing (main)

import Html exposing (..)


input : String
input =
    "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."


parsedInput : List Int
parsedInput =
    List.map parse <| String.toList input


parse : Char -> Int
parse chr =
    if chr == '.' then
        1
    else
        0


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ input) ]
        , div [] [ text ("Part 1: " ++ (toString <| solve 40 0 1 parsedInput)) ]
        , div [] [ text ("Part 2: " ++ (toString <| solve 400000 0 1 parsedInput)) ]
        ]


solve : Int -> Int -> Int -> List Int -> Int
solve max sum rows current =
    let
        newSum =
            (+) sum <| List.sum current

        nextRows =
            rows + 1
    in
        if rows == max then
            newSum
        else
            solve max newSum nextRows <| nextRow [] <| (::) 1 <| current ++ [ 1 ]


nextRow : List Int -> List Int -> List Int
nextRow result lastRow =
    case lastRow of
        lft :: _ :: rgt :: xs ->
            let
                newResult =
                    if lft == rgt then
                        result ++ [ 1 ]
                    else
                        result ++ [ 0 ]
            in
                nextRow newResult <| List.drop 1 lastRow

        _ ->
            result
