module Day07.Part1 exposing (main)

import Day07.Input exposing (IPv7, parsedInput)
import Html exposing (Html, div, text)


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
            if not identical && (firstHalf == secondHalf) then
                True

            else
                abba (List.drop 1 supernet)

        _ ->
            False


abbaIn : List String -> Bool
abbaIn list =
    case list of
        [] ->
            False

        x :: xs ->
            if abba <| String.toList x then
                True

            else
                abbaIn xs


tlsSupport : IPv7 -> Bool
tlsSupport { supernets, hypernets } =
    (not <| abbaIn hypernets) && abbaIn supernets


solve : List IPv7 -> Int
solve =
    List.filter tlsSupport >> List.length


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ (String.fromInt <| solve parsedInput))
            ]
        ]
