module Day20.Day20 exposing (main)

import Day20.Input exposing (parsedInput, rawInput)
import Html exposing (Html, div, text)
import Set exposing (Set)


print : Maybe Int -> String
print =
    Maybe.map String.fromInt >> Maybe.withDefault "No answer"


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (print <| List.head finalWhitelist)) ]
        , div [] [ text ("Part 2: " ++ (String.fromInt <| List.length finalWhitelist)) ]
        ]


finalWhitelist : List Int
finalWhitelist =
    whitelist parsedInput 0 Set.empty


whitelist : List ( Int, Int ) -> Int -> Set Int -> List Int
whitelist blacklists idx =
    case blacklists of
        ( l, r ) :: xs ->
            if l > idx then
                Set.insert idx >> whitelist blacklists (idx + 1)

            else if r > idx then
                whitelist xs (r + 1)

            else
                whitelist xs idx

        _ ->
            Set.toList
