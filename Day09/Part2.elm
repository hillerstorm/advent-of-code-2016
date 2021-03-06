module Day09.Part2 exposing (main)

import Day09.Input exposing (rawInput)
import Html exposing (Html, div, text)


parse : Int -> String -> Int
parse count chars =
    case String.uncons chars of
        Just ( '(', xs ) ->
            let
                ( newCount, newChars ) =
                    parseMarker xs
                        |> endParenIdx xs 0
                        |> Maybe.withDefault ( 1, xs )
            in
            parse (count + newCount) newChars

        Just ( _, xs ) ->
            parse (count + 1) xs

        Nothing ->
            count


endParenIdx : String -> Int -> (Int -> Maybe ( Int, String )) -> Maybe ( Int, String )
endParenIdx str idx f =
    case String.uncons str of
        Just ( ')', _ ) ->
            f idx

        Just ( _, xs ) ->
            endParenIdx xs (idx + 1) f

        _ ->
            Nothing


parseMarker : String -> Int -> Maybe ( Int, String )
parseMarker chars idx =
    case String.split "x" <| String.left idx chars of
        [ a, b ] ->
            Maybe.map2
                (\ax bx ->
                    if bx == 1 then
                        ( 0, String.dropLeft (idx + 1) chars )

                    else
                        let
                            fromIdx =
                                idx + 1

                            toIdx =
                                fromIdx + ax

                            newChars =
                                String.dropLeft toIdx chars
                        in
                        if ax < 6 then
                            ( ax * bx, newChars )

                        else
                            let
                                chrs =
                                    String.slice fromIdx toIdx chars

                                newCount =
                                    if String.contains "(" chrs then
                                        parse 0 <| String.repeat bx chrs

                                    else
                                        ax * bx
                            in
                            ( newCount, newChars )
                )
                (String.toInt a)
                (String.toInt b)

        _ ->
            Nothing


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ (String.fromInt <| parse 0 rawInput))
            ]
        ]
