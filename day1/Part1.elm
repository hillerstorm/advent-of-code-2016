module Part1 exposing (..)

import Html exposing (..)


input : List Move
input =
    [ R 2, L 3, R 2, R 4, L 2, L 1, R 2, R 4, R 1, L 4, L 5, R 5, R 5, R 2, R 2, R 1, L 2, L 3, L 2, L 1, R 3, L 5, R 187, R 1, R 4, L 1, R 5, L 3, L 4, R 50, L 4, R 2, R 70, L 3, L 2, R 4, R 3, R 194, L 3, L 4, L 4, L 3, L 4, R 4, R 5, L 1, L 5, L 4, R 1, L 2, R 4, L 5, L 3, R 4, L 5, L 5, R 5, R 3, R 5, L 2, L 4, R 4, L 1, R 3, R 1, L 1, L 2, R 2, R 2, L 3, R 3, R 2, R 5, R 2, R 5, L 3, R 2, L 5, R 1, R 2, R 2, L 4, L 5, L 1, L 4, R 4, R 3, R 1, R 2, L 1, L 2, R 4, R 5, L 2, R 3, L 4, L 5, L 5, L 4, R 4, L 2, R 1, R 1, L 2, L 3, L 2, R 2, L 4, R 3, R 2, L 1, L 3, L 2, L 4, L 4, R 2, L 3, L 3, R 2, L 4, L 3, R 4, R 3, L 2, L 1, L 4, R 4, R 2, L 4, L 4, L 5, L 1, R 2, L 5, L 2, L 3, R 2, L 2 ]


type Move
    = L Int
    | R Int


solve : List Move -> Position -> Direction -> Int
solve moves position direction =
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
                solve rest nextPosition nextDirection


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
        [ div [] [ text ("Input: " ++ (toString input)) ]
        , div [] [ text ("Result: " ++ (toString (solve input startPosition North))) ]
        ]
