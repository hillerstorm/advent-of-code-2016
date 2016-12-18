module Day7.Part1 exposing (main)

import Html exposing (..)
import Day7.Input exposing (rawInput, parsedInput, IPv7)


abba : List Char -> Bool
abba supernet =
    case supernet of
        a :: b :: c :: d :: _ ->
            let
                firstHalf =
                    [ a, b ]

                secondHalf =
                    [ d, c ]

                identical =
                    a == b && b == c && c == d
            in
                (not identical) && (firstHalf == secondHalf) || (abba <| List.drop 1 supernet)

        _ ->
            False


abbaIn : List String -> Bool
abbaIn list =
    case list of
        [] ->
            False

        x :: xs ->
            (abba <| String.toList x) || abbaIn xs


tlsSupport : IPv7 -> Bool
tlsSupport { supernets, hypernets } =
    (not <| abbaIn hypernets) && abbaIn supernets


solve : List IPv7 -> Int
solve =
    List.length << List.filter tlsSupport


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| solve parsedInput)) ]
        ]
