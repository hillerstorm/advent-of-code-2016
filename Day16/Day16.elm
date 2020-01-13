module Day16.Day16 exposing (main)

import Html exposing (Html, div, text)


input : String
input =
    "10111100110001111"


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (calcChecksum "" <| dragon 272 input)) ]
        , div [] [ text ("Part 2: " ++ (calcChecksum "" <| dragon 35651584 input)) ]
        ]


calcChecksum : String -> String -> String
calcChecksum checksum str =
    if String.length str == 0 then
        if modBy 2 (String.length checksum) == 0 then
            calcChecksum "" checksum

        else
            checksum

    else
        let
            newChecksum =
                checksum ++ checkPair str
        in
        calcChecksum newChecksum (String.dropLeft 2 str)


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
                String.foldl String.cons "" str
        in
        dragon maxLength (str ++ "0" ++ rightPart)
