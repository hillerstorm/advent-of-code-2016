module Day1.Part1 exposing (main)

import Html exposing (..)
import Tuple exposing (mapFirst, mapSecond)
import Day1.Input exposing (rawInput, Move(..), parsedInput)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ (rawInput)) ]
        , div [] [ text ("Result: " ++ (toString <| solve North parsedInput ( 0, 0 ))) ]
        ]


solve : Direction -> List Move -> Position -> Int
solve direction moves =
    case moves of
        [] ->
            distance

        move :: rest ->
            let
                nextDirection =
                    turn move direction

                steps =
                    blocks move
            in
                solve nextDirection rest << moveForward nextDirection steps


type Direction
    = North
    | East
    | South
    | West


type alias Position =
    ( Int, Int )


distance : Position -> Int
distance ( x, y ) =
    (abs x) + (abs y)


turn : Move -> Direction -> Direction
turn move direction =
    case ( move, direction ) of
        ( L _, North ) ->
            West

        ( L _, East ) ->
            North

        ( L _, South ) ->
            East

        ( L _, West ) ->
            South

        ( R _, South ) ->
            West

        ( R _, West ) ->
            North

        ( R _, North ) ->
            East

        ( R _, East ) ->
            South


blocks : Move -> Int
blocks move =
    case move of
        L x ->
            x

        R x ->
            x


moveForward : Direction -> Int -> Position -> Position
moveForward direction =
    case direction of
        North ->
            mapSecond << (+)

        South ->
            mapSecond << flip (-)

        East ->
            mapFirst << (+)

        West ->
            mapFirst << flip (-)
