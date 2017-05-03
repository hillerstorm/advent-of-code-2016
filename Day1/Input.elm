module Day1.Input exposing (rawInput, Move(..), parsedInput)


rawInput : String
rawInput =
    "R2, L3, R2, R4, L2, L1, R2, R4, R1, L4, L5, R5, R5, R2, R2, R1, L2, L3, L2, L1, R3, L5, R187, R1, R4, L1, R5, L3, L4, R50, L4, R2, R70, L3, L2, R4, R3, R194, L3, L4, L4, L3, L4, R4, R5, L1, L5, L4, R1, L2, R4, L5, L3, R4, L5, L5, R5, R3, R5, L2, L4, R4, L1, R3, R1, L1, L2, R2, R2, L3, R3, R2, R5, R2, R5, L3, R2, L5, R1, R2, R2, L4, L5, L1, L4, R4, R3, R1, R2, L1, L2, R4, R5, L2, R3, L4, L5, L5, L4, R4, L2, R1, R1, L2, L3, L2, R2, L4, R3, R2, L1, L3, L2, L4, L4, R2, L3, L3, R2, L4, L3, R4, R3, L2, L1, L4, R4, R2, L4, L4, L5, L1, R2, L5, L2, L3, R2, L2"


parsedInput : List Move
parsedInput =
    parse rawInput


type Move
    = L Int
    | R Int


parse : String -> List Move
parse =
    List.foldl parseMove [] << String.split ", "


parseMove : String -> List Move -> List Move
parseMove move =
    case String.uncons move of
        Just ( 'R', steps ) ->
            parseSteps steps R

        Just ( 'L', steps ) ->
            parseSteps steps L

        _ ->
            identity


parseSteps : String -> (Int -> Move) -> List Move -> List Move
parseSteps steps f list =
    String.toInt steps
        |> Result.map (\x -> list ++ [ (f x) ])
        |> Result.withDefault list
