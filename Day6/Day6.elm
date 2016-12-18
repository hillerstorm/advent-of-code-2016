module Day6.Day6 exposing (main)

import Html exposing (..)
import Array exposing (fromList, get)
import Set exposing (fromList, toList)
import Regex exposing (HowMany(All), find, regex)
import Day6.Input exposing (rawInput, parsedInput)


rexpSort : String -> Char -> Int
rexpSort column char =
    List.length <| find All (regex <| String.fromChar char) column


nextChar : List Char -> Bool -> Maybe Char
nextChar column reverse =
    case column of
        [] ->
            Nothing

        _ ->
            let
                sortFunc =
                    rexpSort <| String.fromList column

                unique =
                    Set.toList <| Set.fromList column

                sorted =
                    List.sortBy sortFunc unique
            in
                if reverse then
                    List.head <| List.reverse sorted
                else
                    List.head sorted


getColumn : Int -> String -> Maybe Char
getColumn index =
    get index << Array.fromList << String.toList


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
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Part 1: " ++ (solve parsedInput "" 0 True)) ]
        , div [] [ text ("Part 2: " ++ (solve parsedInput "" 0 False)) ]
        ]
