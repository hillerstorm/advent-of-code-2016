module Day17.Day17 exposing (main)

import Html exposing (Html, div, text)
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


noAnswerDefault : Maybe String -> String
noAnswerDefault =
    Maybe.withDefault "No answer"


print : Maybe Int -> String
print =
    Maybe.map String.fromInt >> noAnswerDefault


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (noAnswerDefault <| solve Shortest Nothing [ ( start, [] ) ])) ]
        , div [] [ text ("Part 2: " ++ (print <| Maybe.map String.length <| solve Longest Nothing [ ( start, [] ) ])) ]
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

        (( ( code, ( x, y ) as position ) as current, rest ) as path) :: xs ->
            if position == goal then
                let
                    nextPath =
                        extractPath path
                in
                case part of
                    Shortest ->
                        Just nextPath

                    Longest ->
                        let
                            newResult =
                                case result of
                                    Nothing ->
                                        nextPath

                                    Just res ->
                                        if String.length nextPath > String.length res then
                                            nextPath

                                        else
                                            res
                        in
                        solve part (Just newResult) xs

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
                                        xs ++ List.map (\a -> ( a, newRest )) nb

                                    nextPath =
                                        List.sortWith (sortByCode part) newPaths
                                in
                                solve part result nextPath

                    _ ->
                        result


hex : String -> List Char
hex =
    MD5.hex >> String.left 4 >> String.toList


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
    Tuple.first >> Tuple.first >> String.length


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
                neighbors code xs (result ++ [ ( newCode, ( x, y ) ) ])


wallChars : List Char
wallChars =
    [ 'b', 'c', 'd', 'e', 'f' ]


isWall : Char -> Bool
isWall chr =
    not <| List.member chr wallChars
