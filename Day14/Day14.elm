module Day14.Day14 exposing (main)

import Html exposing (Html, div, text)
import MD5
import Regex


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ input) ]
        , div [] [ text ("Part 1: " ++ partOne) ]
        , div [] [ text ("Part 2: " ++ partTwo) ]
        ]


input : String
input =
    "ngcjuoqr"


partOne : String
partOne =
    find (baseHashes 1) [] 1 1001
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "No answer"


partTwo : String
partTwo =
    find (baseHashes 2017) [] 2017 1001
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "No answer"


baseHashes : Int -> List ( Int, String )
baseHashes hashCount =
    List.range 0 1000
        |> List.map (mapIndex hashCount)


mapIndex : Int -> Int -> ( Int, String )
mapIndex hashCount x =
    ( x, getHash hashCount <| input ++ String.fromInt x )


getHash : Int -> String -> String
getHash hashCount x =
    if hashCount > 0 then
        getHash (hashCount - 1) <| MD5.hex x

    else
        x


find : List ( Int, String ) -> List Int -> Int -> Int -> Maybe Int
find hashes result hashCount index =
    if List.length result == 64 then
        List.head result

    else
        case hashes of
            [] ->
                List.head result

            ( idx, hash ) :: xs ->
                let
                    nextHashes =
                        xs ++ [ mapIndex hashCount index ]

                    newResult =
                        findThree hash
                            |> Maybe.map (mapFive idx result xs)
                            |> Maybe.withDefault result
                in
                find nextHashes newResult hashCount <| index + 1


mapFive : Int -> List Int -> List ( Int, String ) -> String -> List Int
mapFive idx result hashes char =
    let
        match =
            String.repeat 5 char
    in
    if List.any (Tuple.second >> String.contains match) hashes then
        idx :: result

    else
        result


three : Regex.Regex
three =
    Regex.fromString "(.)\\1\\1"
        |> Maybe.withDefault Regex.never


findThree : String -> Maybe String
findThree =
    Regex.findAtMost 1 three
        >> List.head
        >> Maybe.map (.submatches >> List.filterMap identity >> List.head)
        >> Maybe.withDefault Nothing
