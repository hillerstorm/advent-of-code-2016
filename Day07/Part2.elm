module Day07.Part2 exposing (main)

import Day07.Input exposing (IPv7, parsedInput)
import Html exposing (Html, div, text)


bab : Char -> Char -> List Char -> Bool
bab a b hypernet =
    case hypernet of
        x :: y :: z :: _ ->
            if x == b && y == a && z == b then
                True

            else
                bab a b (List.drop 1 hypernet)

        _ ->
            False


babIn : List String -> Char -> Char -> Bool
babIn hypernets a b =
    case hypernets of
        [] ->
            False

        x :: xs ->
            if bab a b <| String.toList x then
                True

            else
                babIn xs a b


aba : List String -> List Char -> Bool
aba hypernets supernet =
    case supernet of
        a :: b :: c :: _ ->
            let
                match =
                    a == c && a /= b
            in
            if match && babIn hypernets a b then
                True

            else
                aba hypernets (List.drop 1 supernet)

        _ ->
            False


abaIn : List String -> List String -> Bool
abaIn supernets hypernets =
    case supernets of
        [] ->
            False

        x :: xs ->
            if aba hypernets <| String.toList x then
                True

            else
                abaIn xs hypernets


sslSupport : IPv7 -> Bool
sslSupport { supernets, hypernets } =
    abaIn supernets hypernets


solve : List IPv7 -> Int
solve =
    List.filter sslSupport >> List.length


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ (String.fromInt <| solve parsedInput))
            ]
        ]
