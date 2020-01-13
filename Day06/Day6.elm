module Day06.Day6 exposing (main)

import Array
import Day06.Input exposing (parsedInput)
import Html exposing (Html, div, text)
import Regex
import Set


rexpSort : String -> Char -> Int
rexpSort column char =
    Regex.find
        (Regex.fromString (String.fromChar char)
            |> Maybe.withDefault Regex.never
        )
        column
        |> List.length


nextChar : List Char -> Bool -> Maybe Char
nextChar column reverse =
    case column of
        [] ->
            Nothing

        _ ->
            let
                sortFunc =
                    String.fromList column
                        |> rexpSort

                unique =
                    Set.fromList column
                        |> Set.toList

                sorted =
                    List.sortBy sortFunc unique
            in
            if reverse then
                List.reverse sorted
                    |> List.head

            else
                List.head sorted


getColumn : Int -> String -> Maybe Char
getColumn index =
    String.toList >> Array.fromList >> Array.get index


solve : List String -> String -> Int -> Bool -> String
solve input result index reverse =
    let
        column =
            List.filterMap (getColumn index) input
    in
    case nextChar column reverse of
        Just c ->
            solve input (result ++ String.fromChar c) (index + 1) reverse

        Nothing ->
            result


main : Html msg
main =
    div []
        [ div []
            [ text ("Part 1: " ++ solve parsedInput "" 0 True)
            ]
        , div []
            [ text ("Part 2: " ++ solve parsedInput "" 0 False)
            ]
        ]
