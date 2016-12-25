module Day10.Day10 exposing (main)

import Html exposing (..)
import Dict exposing (..)
import Day10.Input exposing (rawInput, parsedInput, Out(..))


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
                            |> ((,,) outputs <| Dict.remove bot bots)
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
insertInputs ( inputs, instructions ) values =
    case inputs of
        [] ->
            values

        x :: xs ->
            insertInputs ( xs, instructions ) <| execute x instructions values


solved : ( Maybe Int, Maybe Int )
solved =
    let
        ( outputs, _, bot ) =
            insertInputs parsedInput ( Dict.empty, Dict.empty, Nothing )
    in
        case ( Dict.get 0 outputs, Dict.get 1 outputs, Dict.get 2 outputs ) of
            ( Just a, Just b, Just c ) ->
                ( bot, Just (a * b * c) )

            _ ->
                ( bot, Nothing )


main : Html msg
main =
    let
        ( firstPart, secondPart ) =
            solved
    in
        div []
            [ div [] [ text ("Input: " ++ rawInput) ]
            , div [] [ text ("Part 1: " ++ (toString firstPart)) ]
            , div [] [ text ("Part 2: " ++ (toString secondPart)) ]
            ]
