module Day1Part1 exposing (..)

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
        [ div [] [ text ("Input: " ++ (toString input)) ]
        , div [] [ text ("Result: " ++ (parse input |> solve startPosition North |> toString)) ]
        ]
