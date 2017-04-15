module Day11.Input exposing (rawInput, parsedInput)

import Regex exposing (HowMany(All), find, regex)


rawInput : String
rawInput =
    """The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
The fourth floor contains nothing relevant."""


parsedInput : Floors
parsedInput =
    parse rawInput


type alias Floors =
    ( Floor, Floor, Floor, Floor )


type alias Floor =
    List Part


type Part
    = Generator String
    | Microchip String


parse : String -> Floors
parse =
    List.foldl map ( [], [], [], [] ) << String.lines


map : String -> Floors -> Floors
map line floors =
    case List.drop 1 <| String.words line of
        [ _, "floor", "contains", "nothing", "relevant" ] ->
            floors

        floor :: "floor" :: "contains" :: rest ->
            let
                ( first, second, third, fourth ) =
                    floors

                str =
                    String.join " " rest

                generators =
                    getParts (\x -> Generator x) genRegex str

                microchips =
                    getParts (\x -> Microchip x) chipRegex str

                parts =
                    generators ++ microchips
            in
                case floor of
                    "first" ->
                        ( parts, second, third, fourth )

                    "second" ->
                        ( first, parts, third, fourth )

                    "third" ->
                        ( first, second, parts, fourth )

                    "fourth" ->
                        ( first, second, third, parts )

                    _ ->
                        floors

        _ ->
            floors


getParts : (String -> Part) -> Regex.Regex -> String -> List Part
getParts f pattern str =
    find All pattern str
        |> List.map .submatches
        |> List.concat
        |> List.filterMap identity
        |> List.map f


genRegex : Regex.Regex
genRegex =
    regex "(\\w+) generator"


chipRegex : Regex.Regex
chipRegex =
    regex "(\\w+)-compatible"
