module Day03.Part2 exposing (main)

import Day03.Input exposing (parsedInput, rawInput)
import Html exposing (Html, div, text)


partitionValues : List ( Int, Int ) -> List Int
partitionValues list =
    let
        ( zeros, onesAndTwos ) =
            List.partition (Tuple.first >> (==) 0) list

        ( ones, twos ) =
            List.partition (Tuple.first >> (==) 1) onesAndTwos
    in
    Tuple.second <| List.unzip <| zeros ++ ones ++ twos


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
solve =
    List.filter (\x -> List.length x == 3)
        >> List.map (List.indexedMap Tuple.pair)
        >> List.concat
        >> partitionValues
        >> checkValid 0


main : Html msg
main =
    div []
        [ div []
            [ text ("Input: " ++ rawInput)
            ]
        , div []
            [ text ("Result: " ++ (String.fromInt <| solve parsedInput))
            ]
        ]
