module Day7.Part2 exposing (main)

import Html exposing (..)
import Day7.Input exposing (rawInput, parsedInput, IPv7)


bab : Char -> Char -> List Char -> Bool
bab a b hypernet =
    case hypernet of
        x :: y :: z :: _ ->
            x == b && y == a && z == b || (bab a b <| List.drop 1 hypernet)

        _ ->
            False


babIn : List String -> Char -> Char -> Bool
babIn hypernets a b =
    case hypernets of
        [] ->
            False

        x :: xs ->
            (bab a b <| String.toList x) || babIn xs a b


aba : List String -> List Char -> Bool
aba hypernets supernet =
    case supernet of
        a :: b :: c :: _ ->
            let
                match =
                    a == c && a /= b
            in
                match && (babIn hypernets a b) || (aba hypernets <| List.drop 1 supernet)

        _ ->
            False


abaIn : List String -> List String -> Bool
abaIn supernets hypernets =
    case supernets of
        [] ->
            False

        x :: xs ->
            (aba hypernets <| String.toList x) || abaIn xs hypernets


sslSupport : IPv7 -> Bool
sslSupport { supernets, hypernets } =
    abaIn supernets hypernets


solve : List IPv7 -> Int
solve =
    List.length << List.filter sslSupport


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| solve parsedInput)) ]
        ]
