module Day04.Part1 exposing (main, validRoom)

import Char exposing (isLower)
import Day04.Input exposing (Room, parsedInput)
import Html exposing (Html, div, text)


unique : List Char -> List Char -> List Char
unique result list =
    case list of
        [] ->
            result

        x :: xs ->
            if List.member x result then
                unique result xs

            else
                unique (x :: result) xs


count : Char -> String -> Int
count chr =
    String.filter ((==) chr) >> String.length


compareChars : String -> Char -> Char -> Order
compareChars name a b =
    let
        aCount =
            count a name

        bCount =
            count b name
    in
    case compare bCount aCount of
        EQ ->
            compare a b

        diff ->
            diff


validRoom : Room -> Bool
validRoom room =
    let
        calcChecksum =
            String.filter isLower room.name
                |> String.toList
                |> unique []
                |> List.sort
                |> List.sortWith (compareChars room.name)
                |> List.take 5
                |> String.fromList
    in
    room.checksum == calcChecksum


filterValid : Room -> Maybe Int
filterValid room =
    if validRoom room then
        Just room.sector

    else
        Nothing


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ (String.fromInt <| List.sum <| List.filterMap filterValid parsedInput))
            ]
        ]
