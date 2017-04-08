module Day20.Main exposing (main)

import Set exposing (..)
import Html exposing (..)
import Day20.Input exposing (rawInput, parsedInput)


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (toString <| List.head finalWhitelist)) ]
        , div [] [ text ("Part 2: " ++ (toString <| List.length finalWhitelist)) ]
        ]


finalWhitelist : List Int
finalWhitelist =
    whitelist parsedInput 0 Set.empty


whitelist : List ( Int, Int ) -> Int -> Set Int -> List Int
whitelist blacklists idx =
    case blacklists of
        ( l, r ) :: xs ->
            if l > idx then
                whitelist blacklists (idx + 1) << Set.insert idx
            else if r > idx then
                whitelist xs (r + 1)
            else
                whitelist xs idx

        _ ->
            Set.toList
