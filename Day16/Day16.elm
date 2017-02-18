module Day15.Day15 exposing (main)

import Html exposing (..)


input : String
input =
    "10111100110001111"


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ input) ]
        , div [] [ text ("Part 1: " ++ (calcChecksum "" <| dragon 272 input)) ]
        , div [] [ text ("Part 2: " ++ (calcChecksum "" <| dragon 35651584 input)) ]
        ]


calcChecksum : String -> String -> String
calcChecksum checksum str =
    if String.length str == 0 then
        if String.length checksum % 2 == 0 then
            calcChecksum "" checksum
        else
            checksum
    else
        let
            newChecksum =
                (++) checksum <| checkPair str
        in
            calcChecksum newChecksum <| String.dropLeft 2 str


checkPair : String -> String
checkPair str =
    case String.split "" <| String.left 2 str of
        [ a, b ] ->
            if a == b then
                "1"
            else
                "0"

        _ ->
            ""


dragon : Int -> String -> String
dragon maxLength str =
    if String.length str >= maxLength then
        String.left maxLength str
    else
        let
            rightPart =
                String.foldl reverseString "" str
        in
            dragon maxLength <| str ++ "0" ++ rightPart


reverseString : Char -> String -> String
reverseString =
    (++) << flip


flip : Char -> String
flip chr =
    if chr == '0' then
        "1"
    else
        "0"
