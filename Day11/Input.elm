module Day11.Input exposing (parsedInput, rawInput)

import Regex exposing (Regex)


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
    { first : Floor
    , second : Floor
    , third : Floor
    , fourth : Floor
    }


type alias Floor =
    List Part


type Part
    = Generator String
    | Microchip String


initialFloors : Floors
initialFloors =
    { first = []
    , second = []
    , third = []
    , fourth = []
    }


parse : String -> Floors
parse =
    String.lines >> List.foldl map initialFloors


map : String -> Floors -> Floors
map line floors =
    case List.drop 1 <| String.words line of
        [ _, "floor", "contains", "nothing", "relevant" ] ->
            floors

        floor :: "floor" :: "contains" :: rest ->
            let
                str =
                    String.join " " rest

                generators =
                    getParts Generator genRegex str

                microchips =
                    getParts Microchip chipRegex str

                parts =
                    generators ++ microchips
            in
            case floor of
                "first" ->
                    { floors
                        | first = parts
                    }

                "second" ->
                    { floors
                        | second = parts
                    }

                "third" ->
                    { floors
                        | third = parts
                    }

                "fourth" ->
                    { floors
                        | fourth = parts
                    }

                _ ->
                    floors

        _ ->
            floors


getParts : (String -> Part) -> Regex -> String -> List Part
getParts f pattern =
    Regex.find pattern
        >> List.map .submatches
        >> List.concat
        >> List.filterMap identity
        >> List.map f


genRegex : Regex
genRegex =
    Regex.fromString "(\\w+) generator"
        |> Maybe.withDefault Regex.never


chipRegex : Regex
chipRegex =
    Regex.fromString "(\\w+)-compatible"
        |> Maybe.withDefault Regex.never
