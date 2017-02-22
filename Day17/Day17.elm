module Day17.Day17 exposing (main)

import Html exposing (..)
import MD5


input : String
input =
    "yjjvjgan"


inputLength : Int
inputLength =
    String.length input


start : Position
start =
    ( input, ( 0, 0 ) )


type alias Path =
    ( Position, List Position )


type alias Position =
    ( String, Coordinate )


type alias Coordinate =
    ( Int, Int )


goal : Coordinate
goal =
    ( 3, 3 )


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ input) ]
        , div [] [ text ("Part 1: " ++ (toString <| solve Shortest Nothing [ ( start, [] ) ])) ]
        , div [] [ text ("Part 2: " ++ (toString <| Maybe.map String.length <| solve Longest Nothing [ ( start, [] ) ])) ]
        ]


type Part
    = Shortest
    | Longest


extractPath : Path -> String
extractPath =
    String.dropLeft inputLength << Tuple.first << Tuple.first


solve : Part -> Maybe String -> List Path -> Maybe String
solve part result paths =
    case paths of
        [] ->
            result

        path :: xs ->
            findPath part result path xs


findPath : Part -> Maybe String -> Path -> List Path -> Maybe String
findPath part result (( ( code, ( x, y ) as position ) as current, rest ) as path) xs =
    if position == goal then
        withGoal part result xs <| extractPath path
    else
        case hex code of
            [ up, down, left, right ] ->
                let
                    nb =
                        neighbors
                            code
                            [ ( isWall up, ( "U", ( x, y - 1 ) ) )
                            , ( isWall down, ( "D", ( x, y + 1 ) ) )
                            , ( isWall left, ( "L", ( x - 1, y ) ) )
                            , ( isWall right, ( "R", ( x + 1, y ) ) )
                            ]
                            []
                in
                    case nb of
                        [] ->
                            solve part result xs

                        _ ->
                            let
                                newRest =
                                    current :: rest

                                newPaths =
                                    xs ++ (List.map (\x -> ( x, newRest )) nb)
                            in
                                solve part result <| List.sortWith (sortByCode part) newPaths

            _ ->
                result


withGoal : Part -> Maybe String -> List Path -> String -> Maybe String
withGoal part result xs path =
    case part of
        Shortest ->
            Just path

        Longest ->
            let
                newResult =
                    case result of
                        Nothing ->
                            path

                        Just res ->
                            if String.length path > String.length res then
                                path
                            else
                                res
            in
                solve part (Just newResult) xs


hex : String -> List Char
hex =
    String.toList << String.left 4 << MD5.hex


sortByCode : Part -> Path -> Path -> Order
sortByCode part a b =
    let
        aLen =
            codeLength a

        bLen =
            codeLength b

        compareCodes =
            case part of
                Shortest ->
                    compare

                Longest ->
                    flippedCompare
    in
        compareCodes aLen bLen


flippedCompare : comparable -> comparable -> Order
flippedCompare a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


codeLength : Path -> Int
codeLength =
    String.length << Tuple.first << Tuple.first


neighbors : String -> List ( Bool, Position ) -> List Position -> List Position
neighbors code positions result =
    case positions of
        [] ->
            result

        ( wall, ( direction, ( x, y ) ) ) :: xs ->
            if wall || x < 0 || y < 0 || x > 3 || y > 3 then
                neighbors code xs result
            else
                let
                    newCode =
                        code ++ direction
                in
                    neighbors code xs <| result ++ [ ( newCode, ( x, y ) ) ]


wallChars : List Char
wallChars =
    [ 'b', 'c', 'd', 'e', 'f' ]


isWall : Char -> Bool
isWall chr =
    not <| List.member chr wallChars
