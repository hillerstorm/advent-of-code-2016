module Day1Part2 exposing (..)

import Html exposing (..)


input : List Move
input =
    [ R 2, L 3, R 2, R 4, L 2, L 1, R 2, R 4, R 1, L 4, L 5, R 5, R 5, R 2, R 2, R 1, L 2, L 3, L 2, L 1, R 3, L 5, R 187, R 1, R 4, L 1, R 5, L 3, L 4, R 50, L 4, R 2, R 70, L 3, L 2, R 4, R 3, R 194, L 3, L 4, L 4, L 3, L 4, R 4, R 5, L 1, L 5, L 4, R 1, L 2, R 4, L 5, L 3, R 4, L 5, L 5, R 5, R 3, R 5, L 2, L 4, R 4, L 1, R 3, R 1, L 1, L 2, R 2, R 2, L 3, R 3, R 2, R 5, R 2, R 5, L 3, R 2, L 5, R 1, R 2, R 2, L 4, L 5, L 1, L 4, R 4, R 3, R 1, R 2, L 1, L 2, R 4, R 5, L 2, R 3, L 4, L 5, L 5, L 4, R 4, L 2, R 1, R 1, L 2, L 3, L 2, R 2, L 4, R 3, R 2, L 1, L 3, L 2, L 4, L 4, R 2, L 3, L 3, R 2, L 4, L 3, R 4, R 3, L 2, L 1, L 4, R 4, R 2, L 4, L 4, L 5, L 1, R 2, L 5, L 2, L 3, R 2, L 2 ]


type Move
    = L Int
    | R Int


solve : List Move -> Position -> Direction -> List Position -> Maybe Int
solve moves position direction visited =
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
                        solve rest nextPosition nextDirection nextVisited

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
        [ div [] [ text ("Input: " ++ (toString input)) ]
        , div [] [ text ("Result: " ++ (toString (solve input startPosition North []))) ]
        ]
