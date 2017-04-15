module Day4.Part1 exposing (main, validRoom)

import Html exposing (..)
import Char exposing (isLower)
import Day4.Input exposing (rawInput, parsedInput, Room)


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
    String.length << String.filter ((==) chr)


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
validRoom { name, sector, checksum } =
    let
        calcChecksum =
            name
                |> String.filter isLower
                |> String.toList
                |> unique []
                |> List.sort
                |> List.sortWith (compareChars name)
                |> List.take 5
                |> String.fromList
    in
        checksum == calcChecksum


filterValid : Room -> Maybe Int
filterValid room =
    if validRoom room then
        Just room.sector
    else
        Nothing


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| List.sum <| List.filterMap filterValid parsedInput)) ]
        ]
