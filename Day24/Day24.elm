module Day24.Day24 exposing (main)

import Html exposing (..)
import Dict exposing (..)
import Set exposing (..)
import AStar as AS exposing (findPath, straightLineCost)
import Day24.Input exposing (parsedInput, rawInput, Grid, NumberCell, Position)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Part 1: " ++ (toString <| solve One parsedInput)) ]
        , div [] [ text ("Part 2: " ++ (toString <| solve Two parsedInput)) ]
        ]


type Part
    = One
    | Two


solve : Part -> Grid -> Maybe Int
solve part grid =
    let
        ( zero, rest ) =
            List.partition isZero grid.numbers
    in
        case zero of
            [ x ] ->
                let
                    mapFun =
                        case part of
                            One ->
                                (::) x

                            Two ->
                                (\y -> x :: y ++ [ x ])

                    combinations =
                        List.map mapFun <| permutations rest
                in
                    findShortest grid.walls combinations Dict.empty Nothing

            _ ->
                Nothing


permutations : List a -> List (List a)
permutations xs0 =
    xs0 :: perms xs0 []


perms : List a -> List a -> List (List a)
perms tts is =
    case tts of
        [] ->
            []

        t :: ts ->
            let
                interleave xs r =
                    let
                        ( _, zs ) =
                            interleaveX identity xs r
                    in
                        zs

                interleaveX f yys r =
                    case yys of
                        [] ->
                            ( ts, r )

                        y :: ys ->
                            let
                                ( us, zs ) =
                                    interleaveX (f << ((::) y)) ys r
                            in
                                ( y :: us, f (t :: y :: us) :: zs )
            in
                List.foldr interleave (perms ts (t :: is)) (permutations is)


findShortest : Set Position -> List (List NumberCell) -> Dict ( Int, Int ) Int -> Maybe Int -> Maybe Int
findShortest walls cells cache shortest =
    case cells of
        [] ->
            shortest

        x :: xs ->
            let
                ( len, newCache ) =
                    traverse walls cache x 0
            in
                case len of
                    Nothing ->
                        findShortest walls xs newCache shortest

                    Just p ->
                        findShortest walls xs newCache <| Just <| min p <| Maybe.withDefault p shortest


traverse : Set Position -> Dict ( Int, Int ) Int -> List NumberCell -> Int -> ( Maybe Int, Dict ( Int, Int ) Int )
traverse walls cache path result =
    case path of
        [] ->
            ( Just result, cache )

        ( xPos, xNum ) :: y :: xs ->
            let
                ( yPos, yNum ) =
                    y

                newPath =
                    y :: xs

                cached =
                    Dict.get ( xNum, yNum ) cache
            in
                case cached of
                    Nothing ->
                        let
                            len =
                                AS.findPath AS.straightLineCost (availableMoves walls) xPos yPos
                                    |> Maybe.map List.length
                        in
                            case len of
                                Nothing ->
                                    ( Nothing, cache )

                                Just x ->
                                    let
                                        newCache =
                                            cache
                                                |> Dict.insert ( xNum, yNum ) x
                                                |> Dict.insert ( yNum, xNum ) x
                                    in
                                        traverse walls newCache newPath <| result + x

                    Just x ->
                        traverse walls cache newPath <| result + x

        _ ->
            ( Just result, cache )


isZero : NumberCell -> Bool
isZero =
    ((==) 0) << Tuple.second


availableMoves : Set Position -> Position -> Set Position
availableMoves walls ( x, y ) =
    neighbors
        walls
        [ ( x, y - 1 )
        , ( x, y + 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        ]
        Set.empty


neighbors : Set Position -> List Position -> Set Position -> Set Position
neighbors walls positions result =
    case positions of
        [] ->
            result

        pos :: xs ->
            if Set.member pos walls then
                neighbors walls xs result
            else
                neighbors walls xs <| Set.insert pos result
