module Day1.Part1 exposing (main)

import Html exposing (..)
import Day1.Input exposing (rawInput, Move(..), parsedInput)


solve : Position -> Direction -> List Move -> Int
solve position direction moves =
    case moves of
        [] ->
            distance position

        move :: rest ->
            let
                nextDirection =
                    turn move direction

                steps =
                    blocks move

                nextPosition =
                    moveForward position nextDirection steps
            in
                solve nextPosition nextDirection rest


type alias Position =
    { x : Int
    , y : Int
    }


type Direction
    = North
    | East
    | South
    | West


distance : Position -> Int
distance { x, y } =
    (abs x) + (abs y)


turn : Move -> Direction -> Direction
turn move direction =
    case move of
        L _ ->
            case direction of
                North ->
                    West

                East ->
                    North

                South ->
                    East

                West ->
                    South

        R _ ->
            case direction of
                South ->
                    West

                West ->
                    North

                North ->
                    East

                East ->
                    South


blocks : Move -> Int
blocks move =
    case move of
        L x ->
            x

        R x ->
            x


moveForward : Position -> Direction -> Int -> Position
moveForward position direction steps =
    case direction of
        North ->
            { position | y = position.y + steps }

        East ->
            { position | x = position.x + steps }

        South ->
            { position | y = position.y - steps }

        West ->
            { position | x = position.x - steps }


startPosition : Position
startPosition =
    { x = 0
    , y = 0
    }


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ (rawInput)) ]
        , div [] [ text ("Result: " ++ (solve startPosition North parsedInput |> toString)) ]
        ]
