module Day1.Part2 exposing (main)

import Html exposing (..)
import Day1.Input exposing (rawInput, Move(..), parsedInput)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| solve ( 0, 0 ) North [] parsedInput)) ]
        ]


solve : Position -> Direction -> List Position -> List Move -> Maybe Int
solve position direction visited moves =
    case moves of
        [] ->
            Nothing

        move :: rest ->
            let
                nextDirection =
                    turn move direction

                steps =
                    case move of
                        L x ->
                            x

                        R x ->
                            x

                result =
                    moveForward position nextDirection steps visited
            in
                case result of
                    Continue ( nextPosition, nextVisited ) ->
                        solve nextPosition nextDirection nextVisited rest

                    Intersection nextPosition ->
                        Just (distance nextPosition)


type alias Position =
    ( Int, Int )


type Direction
    = North
    | East
    | South
    | West


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


moveForward : Position -> Direction -> Int -> List Position -> Result
moveForward ( x, y ) direction steps visited =
    let
        pos =
            ( x, y )
    in
        if steps == 0 then
            Continue ( pos, pos :: visited )
        else
            let
                nextPosition =
                    case direction of
                        North ->
                            ( x, y + 1 )

                        East ->
                            ( x + 1, y )

                        South ->
                            ( x, y - 1 )

                        West ->
                            ( x - 1, y )

                nextVisited =
                    pos :: visited
            in
                if List.member nextPosition nextVisited then
                    Intersection nextPosition
                else
                    moveForward nextPosition direction (steps - 1) nextVisited


type Result
    = Intersection Position
    | Continue ( Position, List Position )
