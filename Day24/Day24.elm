module Day24.Day24 exposing (main)

import AStar as AS exposing (findPath, straightLineCost)
import Day24.Input exposing (Grid, NumberCell, Position, parsedInput)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Set exposing (Set)


print : Maybe Int -> String
print =
    Maybe.map String.fromInt >> Maybe.withDefault "No answer"


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (print <| solve One parsedInput)) ]
        , div [] [ text ("Part 2: " ++ (print <| solve Two parsedInput)) ]
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
    Maybe.andThen
        (\x ->
            let
                mapFun : List NumberCell -> List NumberCell
                mapFun =
                    case part of
                        One ->
                            (::) x

                        Two ->
                            \y -> x :: y ++ [ x ]

                combinations =
                    permutations rest
                        |> List.map mapFun
            in
            findShortest grid.walls combinations Dict.empty Nothing
        )
        (List.head zero)


permutations : List NumberCell -> List (List NumberCell)
permutations xs0 =
    xs0 :: perms xs0 []


perms : List NumberCell -> List NumberCell -> List (List NumberCell)
perms tts is =
    case tts of
        [] ->
            []

        t :: ts ->
            let
                interleave : List NumberCell -> List (List NumberCell) -> List (List NumberCell)
                interleave xs r =
                    interleaveX identity xs r
                        |> Tuple.second

                interleaveX : (List NumberCell -> List NumberCell) -> List NumberCell -> List (List NumberCell) -> ( List NumberCell, List (List NumberCell) )
                interleaveX f yys r =
                    case yys of
                        [] ->
                            ( ts, r )

                        y :: ys ->
                            let
                                ( us, zs ) =
                                    interleaveX (f << (::) y) ys r
                            in
                            ( y :: us, f (t :: y :: us) :: zs )
            in
            List.foldr interleave (perms ts (t :: is)) (permutations is)


findShortest : Set Position -> List (List NumberCell) -> Dict ( Int, Int ) Int -> Maybe Int -> Maybe Int
findShortest walls cells cache result =
    case cells of
        [] ->
            result

        x :: xs ->
            let
                ( len, newCache ) =
                    traverse walls cache x 0
            in
            case len of
                Nothing ->
                    findShortest walls xs newCache result

                Just p ->
                    let
                        newResult =
                            Just <| (Maybe.withDefault p >> min p) <| result
                    in
                    findShortest walls xs newCache newResult


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
                            traverse walls newCache newPath (result + x)

                Just x ->
                    traverse walls cache newPath (result + x)

        _ ->
            ( Just result, cache )


isZero : NumberCell -> Bool
isZero =
    Tuple.second >> (==) 0


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
                neighbors walls xs (Set.insert pos result)
