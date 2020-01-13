module Day21.Day21 exposing (main)

import Day21.Input exposing (Direction(..), Instruction(..), parsedInput)
import Html exposing (Html, div, text)


password : String
password =
    "abcdefgh"


scrambled : String
scrambled =
    "fbgdceah"


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ solve One parsedInput password) ]
        , div [] [ text ("Part 2: " ++ solve Two (List.reverse parsedInput) scrambled) ]
        ]


solve : Part -> List Instruction -> String -> String
solve part instructions pass =
    case instructions of
        [] ->
            pass

        instruction :: xs ->
            let
                newPass =
                    case instruction of
                        SwapPos x y ->
                            swapPos pass x y

                        SwapLetter x y ->
                            swapLetter x y pass

                        Rotate dir steps ->
                            rotate pass (realDir part dir) steps

                        RotateFrom letter ->
                            rotateFrom part pass letter <| realDir part Right

                        Reverse x y ->
                            reverse pass x y

                        Move x y ->
                            move pass <| actualMove part x y
            in
            solve part xs newPass


type Part
    = One
    | Two


swapPos : String -> Int -> Int -> String
swapPos pass x y =
    let
        minIdx =
            min x y

        maxIdx =
            max x y
    in
    String.join ""
        [ String.left minIdx pass
        , String.left 1 <| String.dropLeft maxIdx pass
        , String.slice (minIdx + 1) maxIdx pass
        , String.left 1 <| String.dropLeft minIdx pass
        , String.dropLeft (maxIdx + 1) pass
        ]


swapLetter : String -> String -> String -> String
swapLetter x y =
    replace x "#" >> replace y x >> replace "#" y


replace : String -> String -> String -> String
replace from to =
    String.split from >> String.join to


rotate : String -> Direction -> Int -> String
rotate pass dir steps =
    if steps < 1 then
        pass

    else
        let
            newPass =
                case dir of
                    Left ->
                        let
                            chr =
                                String.left 1 pass
                        in
                        String.dropLeft 1 pass ++ chr

                    Right ->
                        let
                            chr =
                                String.right 1 pass
                        in
                        chr ++ String.dropRight 1 pass
        in
        rotate newPass dir (steps - 1)


rotateFrom : Part -> String -> String -> Direction -> String
rotateFrom part pass letter dir =
    case List.head <| String.indexes letter pass of
        Just idx ->
            case part of
                One ->
                    rotateFromPartOne pass dir idx

                Two ->
                    rotateFromPartTwo pass dir idx

        Nothing ->
            pass


rotateFromPartOne : String -> Direction -> Int -> String
rotateFromPartOne pass dir idx =
    let
        half =
            String.length pass // 2

        steps =
            if idx >= half then
                idx + 2

            else
                idx + 1
    in
    rotate pass dir steps


rotateFromPartTwo : String -> Direction -> Int -> String
rotateFromPartTwo pass dir idx =
    if idx == 0 then
        rotate pass dir <| String.length pass + 1

    else
        let
            half =
                String.length pass // 2

            steps =
                if isEven idx then
                    getSteps 2 idx isEven <| half + 2

                else
                    getSteps 1 idx isOdd 1
        in
        steps
            |> Maybe.map (rotate pass dir)
            |> Maybe.withDefault pass


getSteps : Int -> Int -> (Int -> Bool) -> Int -> Maybe Int
getSteps low high filterFunc extra =
    List.range low high
        |> List.filter filterFunc
        |> List.indexedMap Tuple.pair
        |> List.reverse
        |> List.head
        |> Maybe.map (Tuple.first >> (+) extra)


isEven : Int -> Bool
isEven i =
    modBy 2 i == 0


isOdd : Int -> Bool
isOdd i =
    modBy 2 i /= 0


reverse : String -> Int -> Int -> String
reverse pass x y =
    String.join ""
        [ String.left x pass
        , String.reverse <| String.slice x (y + 1) pass
        , String.dropLeft (y + 1) pass
        ]


move : String -> ( Int, Int ) -> String
move pass ( x, y ) =
    let
        purged =
            String.join ""
                [ String.left x pass
                , String.dropLeft (x + 1) pass
                ]
    in
    String.join ""
        [ String.left y purged
        , String.slice x (x + 1) pass
        , String.dropLeft y purged
        ]


actualMove : Part -> Int -> Int -> ( Int, Int )
actualMove part x y =
    case part of
        One ->
            ( x, y )

        Two ->
            ( y, x )


realDir : Part -> Direction -> Direction
realDir part direction =
    case part of
        One ->
            direction

        Two ->
            case direction of
                Left ->
                    Right

                Right ->
                    Left
