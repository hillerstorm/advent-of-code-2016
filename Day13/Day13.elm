module Day13.Day13 exposing (main)

import AStar exposing (Position, findPath, straightLineCost)
import Html exposing (Html, div, text)
import ParseInt exposing (toRadix)
import Set exposing (Set)


input : Int
input =
    1350


print : Maybe Int -> String
print =
    Maybe.map String.fromInt >> Maybe.withDefault "No answer"


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (print <| steps ( 31, 39 ))) ]
        , div [] [ text ("Part 2: " ++ (String.fromInt <| part2 intialPositions 0)) ]
        ]


intialPositions : List Position
intialPositions =
    List.range 0 20
        |> List.concatMap (List.map2 Tuple.pair (List.range 0 20) << List.repeat 20)
        |> List.filter (not << isWall)


part2 : List Position -> Int -> Int
part2 positions visited =
    case positions of
        [] ->
            visited

        pos :: xs ->
            case steps pos of
                Nothing ->
                    part2 xs visited

                Just s ->
                    if s <= 50 then
                        part2 xs (visited + 1)

                    else
                        part2 xs visited


steps : Position -> Maybe Int
steps =
    findPath straightLineCost availableMoves ( 1, 1 )
        >> Maybe.map List.length


availableMoves : Position -> Set Position
availableMoves ( x, y ) =
    neighbors
        [ ( x, y - 1 )
        , ( x, y + 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        ]
        Set.empty


neighbors : List Position -> Set Position -> Set Position
neighbors positions result =
    case positions of
        [] ->
            result

        ( x, y ) :: xs ->
            if x < 0 || y < 0 || isWall ( x, y ) then
                neighbors xs result

            else
                neighbors xs (Set.insert ( x, y ) result)


isWall : Position -> Bool
isWall ( x, y ) =
    toRadix 2 (x * x + 3 * x + 2 * x * y + y + y * y + input)
        |> Result.andThen isOdd
        |> Result.withDefault True


isOdd : String -> Result x Bool
isOdd str =
    Ok (modBy 2 (countOnes str) /= 0)


countOnes : String -> Int
countOnes =
    String.filter ((==) '1') >> String.length
