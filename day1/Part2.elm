module Day1Part2 exposing (..)

import Html exposing (..)


input : String
input =
    "R2, L3, R2, R4, L2, L1, R2, R4, R1, L4, L5, R5, R5, R2, R2, R1, L2, L3, L2, L1, R3, L5, R187, R1, R4, L1, R5, L3, L4, R50, L4, R2, R70, L3, L2, R4, R3, R194, L3, L4, L4, L3, L4, R4, R5, L1, L5, L4, R1, L2, R4, L5, L3, R4, L5, L5, R5, R3, R5, L2, L4, R4, L1, R3, R1, L1, L2, R2, R2, L3, R3, R2, R5, R2, R5, L3, R2, L5, R1, R2, R2, L4, L5, L1, L4, R4, R3, R1, R2, L1, L2, R4, R5, L2, R3, L4, L5, L5, L4, R4, L2, R1, R1, L2, L3, L2, R2, L4, R3, R2, L1, L3, L2, L4, L4, R2, L3, L3, R2, L4, L3, R4, R3, L2, L1, L4, R4, R2, L4, L4, L5, L1, R2, L5, L2, L3, R2, L2"


parse : String -> List Move
parse string =
    (String.split ", " string)
        |> List.foldr parseMove []


parseMove : String -> List Move -> List Move
parseMove move list =
    let
        parsedMove =
            String.uncons move
    in
        case parsedMove of
            Just ( chr, steps ) ->
                let
                    result =
                        String.toInt steps
                in
                    case result of
                        Ok x ->
                            case chr of
                                'R' ->
                                    R x :: list

                                'L' ->
                                    L x :: list

                                _ ->
                                    list

                        _ ->
                            list

            Nothing ->
                list


type Move
    = L Int
    | R Int


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
        [ div [] [ text ("Input: " ++ (toString input)) ]
        , div [] [ text ("Result: " ++ (parse input |> solve startPosition North [] |> toString)) ]
        ]
