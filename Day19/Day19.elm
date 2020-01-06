module Day19.Day19 exposing (main)

import Bitwise exposing (or, shiftRightBy)
import Html exposing (Html, div, text)


input : Int
input =
    3004953


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ String.fromInt input) ]
        , div [] [ text ("Part 1: " ++ (String.fromInt <| part1 input)) ]
        , div [] [ text ("Part 2: " ++ (String.fromInt <| part2 input 1)) ]
        ]


part1 : Int -> Int
part1 to =
    let
        from =
            previousPowerByTwo to
    in
    if from == to then
        from

    else
        (to - from) * 2 + 1


previousPowerByTwo : Int -> Int
previousPowerByTwo x =
    let
        y =
            x
                |> orShift 1
                |> orShift 2
                |> orShift 4
                |> orShift 8
                |> orShift 16
    in
    y - shiftRightBy 1 y


orShift : Int -> Int -> Int
orShift by x =
    or x <| shiftRightBy by x


part2 : Int -> Int -> Int
part2 elves max =
    if elves < 3 then
        1

    else if elves == max then
        elves

    else
        let
            nextMax =
                max + max * 2
        in
        if nextMax > elves then
            let
                diff =
                    elves - max
            in
            if diff < max then
                diff

            else
                diff + diff - max

        else
            part2 elves nextMax
