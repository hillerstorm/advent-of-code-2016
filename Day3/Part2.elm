module Day3.Part2 exposing (main)

import Html exposing (..)
import Tuple exposing (second)
import Day3.Input exposing (rawInput, parsedInput)


partitionValues : List ( Int, Int ) -> List Int
partitionValues list =
    let
        ( zeros, onesAndTwos ) =
            List.partition (\( i, _ ) -> i == 0) list

        ( ones, twos ) =
            List.partition (\( i, _ ) -> i == 1) onesAndTwos
    in
        second <| List.unzip <| zeros ++ ones ++ twos


checkValid : Int -> List Int -> Int
checkValid count list =
    case list of
        a :: b :: c :: rest ->
            let
                newCount =
                    if a + b > c && a + c > b && b + c > a then
                        count + 1
                    else
                        count
            in
                checkValid newCount rest

        _ ->
            count


solve : List (List Int) -> Int
solve input =
    input
        |> List.filter (\x -> List.length x == 3)
        |> List.map (List.indexedMap (,))
        |> List.concat
        |> partitionValues
        |> checkValid 0


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| solve parsedInput)) ]
        ]
