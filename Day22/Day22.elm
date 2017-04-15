module Day22.Main exposing (main)

import Html exposing (..)
import Day22.Input exposing (rawInput, parsedInput, Node)


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (toString part1)) ]
        ]


part1 : Int
part1 =
    List.length <| viable parsedInput []


viable : List Node -> List ( Node, Node ) -> List ( Node, Node )
viable nodes =
    case nodes of
        [] ->
            identity

        node :: xs ->
            viable xs << addViable node xs


addViable : Node -> List Node -> List ( Node, Node ) -> List ( Node, Node )
addViable node nodes list =
    case nodes of
        [] ->
            list

        x :: xs ->
            let
                withFirst =
                    if node.used > 0 && node.used <= x.avail then
                        ( node, x ) :: list
                    else
                        list

                withSecond =
                    if x.used > 0 && x.used <= node.avail then
                        ( x, node ) :: withFirst
                    else
                        withFirst
            in
                addViable node xs withSecond
