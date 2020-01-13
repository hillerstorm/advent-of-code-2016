module Day18.Day18 exposing (main)

import Html exposing (Html, div, text)


input : String
input =
    "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."


parsedInput : List Int
parsedInput =
    String.toList input
        |> List.filterMap parse


parse : Char -> Maybe Int
parse chr =
    if chr == '.' then
        Just 1

    else if chr == '^' then
        Just 0

    else
        Nothing


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (String.fromInt <| solve 40 0 1 parsedInput)) ]
        , div [] [ text ("Part 2: " ++ (String.fromInt <| solve 400000 0 1 parsedInput)) ]
        ]


solve : Int -> Int -> Int -> List Int -> Int
solve max sum rows current =
    let
        newSum =
            sum + List.sum current

        nextRows =
            rows + 1
    in
    if rows == max then
        newSum

    else
        let
            nextCurrent =
                nextRow [] <| 1 :: current ++ [ 1 ]
        in
        solve max newSum nextRows nextCurrent


nextRow : List Int -> List Int -> List Int
nextRow result lastRow =
    case lastRow of
        lft :: _ :: rgt :: _ ->
            let
                newResult =
                    if lft == rgt then
                        result ++ [ 1 ]

                    else
                        result ++ [ 0 ]
            in
            nextRow newResult (List.drop 1 lastRow)

        _ ->
            result
