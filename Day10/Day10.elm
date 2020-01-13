module Day10.Day10 exposing (main)

import Day10.Input exposing (Out(..), parsedInput)
import Dict exposing (Dict)
import Html exposing (Html, div, text)


getPartOne : Maybe Int -> Int -> ( Int, Int ) -> Maybe Int
getPartOne current bot comparison =
    case current of
        Just _ ->
            current

        Nothing ->
            if comparison == partOneComparison then
                Just bot

            else
                Nothing


executeBranch : Out -> Int -> Instructions -> ( Outputs, Bots, Maybe Int ) -> ( Outputs, Bots, Maybe Int )
executeBranch out value instructions ( outputs, bots, partOne ) =
    case out of
        Bot x ->
            execute ( x, value ) instructions ( outputs, bots, partOne )

        Output x ->
            ( Dict.insert x value outputs, bots, partOne )


execute : Input -> Instructions -> ( Outputs, Bots, Maybe Int ) -> ( Outputs, Bots, Maybe Int )
execute ( bot, value ) instructions ( outputs, bots, partOne ) =
    case Dict.get bot instructions of
        Just ( lowOut, highOut ) ->
            case Dict.get bot bots of
                Just val ->
                    let
                        ( low, high ) =
                            if val < value then
                                ( val, value )

                            else
                                ( value, val )
                    in
                    getPartOne partOne bot ( low, high )
                        |> ((\b c -> ( outputs, b, c )) <| Dict.remove bot bots)
                        |> executeBranch lowOut low instructions
                        |> executeBranch highOut high instructions

                Nothing ->
                    ( outputs, Dict.insert bot value bots, partOne )

        Nothing ->
            ( outputs, bots, partOne )


type alias Inputs =
    List Input


type alias Input =
    ( Int, Int )


type alias Instructions =
    Dict Int Instruction


type alias Instruction =
    ( Out, Out )


type alias Outputs =
    Dict Int Int


type alias Bots =
    Dict Int Int


partOneComparison : ( Int, Int )
partOneComparison =
    ( 17, 61 )


insertInputs : ( Inputs, Instructions ) -> ( Outputs, Bots, Maybe Int ) -> ( Outputs, Bots, Maybe Int )
insertInputs ( inputs, instructions ) result =
    case inputs of
        [] ->
            result

        x :: xs ->
            insertInputs ( xs, instructions ) (execute x instructions result)


solved : ( Maybe Int, Maybe Int )
solved =
    let
        ( outputs, _, bot ) =
            insertInputs parsedInput ( Dict.empty, Dict.empty, Nothing )
    in
    Maybe.map3
        (\a b c ->
            ( bot, Just <| a * b * c )
        )
        (Dict.get 0 outputs)
        (Dict.get 1 outputs)
        (Dict.get 2 outputs)
        |> Maybe.withDefault ( bot, Nothing )


print : Maybe Int -> String
print =
    Maybe.map String.fromInt >> Maybe.withDefault "No answer"


main : Html msg
main =
    let
        ( firstPart, secondPart ) =
            Tuple.mapBoth print print solved
    in
    div []
        [ div [] [ text ("Part 1: " ++ firstPart) ]
        , div [] [ text ("Part 2: " ++ secondPart) ]
        ]
