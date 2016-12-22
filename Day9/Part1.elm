module Day9.Part1 exposing (main)

import Html exposing (..)
import Day9.Input exposing (rawInput)


parseMarker : String -> String -> ( String, String )
parseMarker str chars =
    case String.indexes ")" chars of
        [] ->
            ( str ++ "(", chars )

        idx :: _ ->
            let
                marker =
                    String.split "x" <| String.left idx chars
            in
                case marker of
                    [ a, b ] ->
                        case ( String.toInt a, String.toInt b ) of
                            ( Ok ax, Ok bx ) ->
                                let
                                    fromIdx =
                                        idx + 1

                                    toIdx =
                                        fromIdx + ax

                                    repeated =
                                        String.repeat bx <| String.slice fromIdx toIdx chars

                                    newChars =
                                        String.dropLeft toIdx chars
                                in
                                    ( str ++ repeated, newChars )

                            _ ->
                                ( str ++ "(", chars )

                    _ ->
                        ( str ++ "(", chars )


parse : String -> String -> String
parse str chars =
    case String.uncons chars of
        Just ( '(', xs ) ->
            let
                ( newStr, rest ) =
                    parseMarker str xs
            in
                parse newStr rest

        Just ( chr, xs ) ->
            parse (str ++ (String.fromChar chr)) xs

        Nothing ->
            str


decompress : String -> String
decompress =
    parse "" << String.concat << String.words


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| String.length <| decompress rawInput)) ]
        ]
