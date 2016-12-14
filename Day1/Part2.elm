module Day1.Part2 exposing (main)

import Html exposing (..)
import Day1.Input exposing (rawInput, Move(..), parsedInput)


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


distance : Position -> Int
distance { x, y } =
    (abs x) + (abs y)


type alias Position =
    { x : Int
    , y : Int
    }


type Direction
    = North
    | East
    | South
    | West


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


moveForward : Position -> Direction -> Int -> List Position -> Result
moveForward position direction steps visited =
    if steps == 0 then
        Continue ( position, position :: visited )
    else
        let
            nextPosition =
                case direction of
                    North ->
                        { position | y = position.y + 1 }

                    East ->
                        { position | x = position.x + 1 }

                    South ->
                        { position | y = position.y - 1 }

                    West ->
                        { position | x = position.x - 1 }

            nextVisited =
                position :: visited
        in
            if List.member nextPosition nextVisited then
                Intersection nextPosition
            else
                moveForward nextPosition direction (steps - 1) nextVisited


type Result
    = Intersection Position
    | Continue ( Position, List Position )


startPosition : Position
startPosition =
    { x = 0
    , y = 0
    }


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (solve startPosition North [] parsedInput |> toString)) ]
        ]
