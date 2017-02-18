module Day15.Day15 exposing (main)

import Html exposing (..)
import Day15.Input exposing (rawInput, parsedInput, Disc)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Part 1: " ++ (toString <| solve parsedInput 0)) ]
        , div [] [ text ("Part 2: " ++ (toString <| solve (parsedInput ++ [ ( 11, 0 ) ]) 0)) ]
        ]


solve : List Disc -> Int -> Int
solve discs time =
    if isOpen discs <| time + 1 then
        time
    else
        solve discs <| time + 1


isOpen : List Disc -> Int -> Bool
isOpen discs time =
    case discs of
        [] ->
            True

        disc :: xs ->
            hasSlot time disc && (isOpen xs <| time + 1)


hasSlot : Int -> Disc -> Bool
hasSlot time ( positions, start ) =
    (start + time) % positions == 0
