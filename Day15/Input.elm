module Day15.Input exposing (Disc, parsedInput, rawInput)


rawInput : String
rawInput =
    """Disc #1 has 17 positions; at time=0, it is at position 5.
Disc #2 has 19 positions; at time=0, it is at position 8.
Disc #3 has 7 positions; at time=0, it is at position 1.
Disc #4 has 13 positions; at time=0, it is at position 7.
Disc #5 has 5 positions; at time=0, it is at position 1.
Disc #6 has 3 positions; at time=0, it is at position 0."""


parsedInput : List Disc
parsedInput =
    parse rawInput


type alias Disc =
    ( Int, Int )


parse : String -> List Disc
parse =
    String.lines >> List.filterMap parseLine


parseLine : String -> Maybe Disc
parseLine line =
    case String.words line of
        [ "Disc", _, "has", positions, "positions;", "at", "time=0,", "it", "is", "at", "position", start ] ->
            Maybe.map2 Tuple.pair (String.toInt positions) (String.toInt <| String.dropRight 1 start)

        _ ->
            Nothing
