module Day22.Day22 exposing (main)

import Day22.Input exposing (Node, parsedInput, rawInput)
import Html exposing (Html, div, text)


main : Html msg
main =
    div []
        [ div []
            [ text ("Part 1: " ++ String.fromInt part1)
            ]
        ]


part1 : Int
part1 =
    viable parsedInput []
        |> List.length


viable : List Node -> List ( Node, Node ) -> List ( Node, Node )
viable nodes =
    case nodes of
        [] ->
            identity

        node :: xs ->
            addViable node xs >> viable xs


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
