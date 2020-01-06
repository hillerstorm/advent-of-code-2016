module Day09.Part1 exposing (main)

import Day09.Input exposing (rawInput)
import Html exposing (Html, div, text)


endParenIdx : String -> Int -> Maybe Int
endParenIdx str idx =
    case String.uncons str of
        Just ( ')', xs ) ->
            Just idx

        Just ( _, xs ) ->
            endParenIdx xs <| idx + 1

        Nothing ->
            Nothing


parseMarker : String -> ( Int, String )
parseMarker chars =
    case endParenIdx chars 0 of
        Just idx ->
            case String.split "x" <| String.left idx chars of
                [ a, b ] ->
                    case ( String.toInt a, String.toInt b ) of
                        ( Just ax, Just bx ) ->
                            let
                                fromIdx =
                                    idx + 1

                                toIdx =
                                    fromIdx + ax

                                newCount =
                                    (toIdx - fromIdx) * bx

                                newChars =
                                    String.dropLeft toIdx chars
                            in
                            ( newCount, newChars )

                        _ ->
                            ( 1, chars )

                _ ->
                    ( 1, chars )

        Nothing ->
            ( 1, chars )


parse : Int -> String -> Int
parse count chars =
    case String.uncons chars of
        Just ( '(', xs ) ->
            let
                ( newCount, newChars ) =
                    parseMarker xs
            in
            parse (count + newCount) newChars

        Just ( _, xs ) ->
            parse (count + 1) xs

        Nothing ->
            count


main : Html msg
main =
    div []
        [ div []
            [ text ("Input: " ++ rawInput)
            ]
        , div []
            [ text ("Result: " ++ (String.fromInt <| parse 0 rawInput))
            ]
        ]
