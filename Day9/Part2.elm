module Day9.Part2 exposing (main)

import Html exposing (..)
import Day9.Input exposing (rawInput)


parseMarker : Int -> String -> ( Int, String )
parseMarker count chars =
    case String.indexes ")" chars of
        [] ->
            ( count + 1, chars )

        idx :: _ ->
            let
                marker =
                    String.split "x" <| String.left idx chars
            in
                case marker of
                    [ a, b ] ->
                        case ( String.toInt a, String.toInt b ) of
                            ( Ok ax, Ok 1 ) ->
                                ( count, String.dropLeft (idx + 1) chars )

                            ( Ok ax, Ok bx ) ->
                                let
                                    fromIdx =
                                        idx + 1

                                    toIdx =
                                        fromIdx + ax
                                in
                                    if ax < 6 then
                                        ( count + (ax * bx), String.dropLeft toIdx chars )
                                    else
                                        let
                                            chrs =
                                                String.slice fromIdx toIdx chars

                                            newCount =
                                                if String.contains "(" chrs then
                                                    parse 0 <| String.repeat bx <| chrs
                                                else
                                                    ax * bx

                                            newChars =
                                                String.dropLeft toIdx chars
                                        in
                                            ( count + newCount, newChars )

                            _ ->
                                ( count + 1, chars )

                    _ ->
                        ( count + 1, chars )


parse : Int -> String -> Int
parse count chars =
    case String.uncons chars of
        Just ( '(', xs ) ->
            let
                ( newCount, rest ) =
                    parseMarker count xs
            in
                parse newCount rest

        Just ( chr, xs ) ->
            parse (count + 1) xs

        Nothing ->
            count


decompress : String -> Int
decompress =
    parse 0 << String.concat << String.words


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| decompress rawInput)) ]
        ]
