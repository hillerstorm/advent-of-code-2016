module Day19 exposing (main)

import Html exposing (..)
import Bitwise exposing (shiftRightBy, or)


input : Int
input =
    3004953


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ (toString input)) ]
        , div [] [ text ("Part 1: " ++ (toString <| part1 input)) ]
        , div [] [ text ("Part 2: " ++ (toString <| part2 input 1)) ]
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
        y - (shiftRightBy 1 y)


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
