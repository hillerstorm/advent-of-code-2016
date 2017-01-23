module Main exposing (..)

import Html exposing (..)
import Set exposing (..)
import ParseInt exposing (toRadix)
import AStar exposing (..)


input : Int
input =
    1350


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ (toString input)) ]
        , div [] [ text ("Part 1: " ++ (toString <| steps ( 31, 39 ))) ]
        , div [] [ text ("Part 2: " ++ (toString <| part2 positions 0)) ]
        ]


positions : List Position
positions =
    List.range 0 20
        |> List.concatMap (List.map2 (,) (List.range 0 20) << List.repeat 20)
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
                        part2 xs <| visited + 1
                    else
                        part2 xs visited


steps : Position -> Maybe Int
steps =
    Maybe.map List.length << findPath straightLineCost availableMoves ( 1, 1 )


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
                neighbors xs <| Set.insert ( x, y ) result


isWall : Position -> Bool
isWall ( x, y ) =
    toRadix 2 (x * x + 3 * x + 2 * x * y + y + y * y + input)
        |> Result.andThen isOdd
        |> Result.withDefault True


isOdd : String -> Result x Bool
isOdd str =
    Ok ((countOnes str) % 2 /= 0)


countOnes : String -> Int
countOnes =
    String.length << String.filter ((==) '1')
