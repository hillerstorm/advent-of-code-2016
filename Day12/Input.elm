module Day12.Input exposing (rawInput, parsedInput, Instruction(..), Value(..), Register(..))


rawInput : String
rawInput =
    """cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 17 c
cpy 18 d
inc a
dec d
jnz d -2
dec c
jnz c -5"""


parsedInput : List Instruction
parsedInput =
    List.filterMap parse <| String.lines rawInput


type Instruction
    = Cpy Value Register
    | Inc Register
    | Dec Register
    | Jnz Value Int


type Value
    = Num Int
    | Reg Register


type Register
    = A
    | B
    | C
    | D


parse : String -> Maybe Instruction
parse line =
    case String.words line of
        [ "cpy", val, reg ] ->
            case ( parseValue val, parseRegister reg ) of
                ( Just value, Just register ) ->
                    Just (Cpy value register)

                _ ->
                    Nothing

        [ "inc", reg ] ->
            case parseRegister reg of
                Just register ->
                    Just (Inc register)

                _ ->
                    Nothing

        [ "dec", reg ] ->
            case parseRegister reg of
                Just register ->
                    Just (Dec register)

                _ ->
                    Nothing

        [ "jnz", val, step ] ->
            case ( parseValue val, String.toInt step ) of
                ( Just value, Ok steps ) ->
                    Just (Jnz value steps)

                _ ->
                    Nothing

        _ ->
            Nothing


parseValue : String -> Maybe Value
parseValue str =
    case parseRegister str of
        Just register ->
            Just (Reg register)

        Nothing ->
            case String.toInt str of
                Ok value ->
                    Just (Num value)

                _ ->
                    Nothing


parseRegister : String -> Maybe Register
parseRegister str =
    case str of
        "a" ->
            Just A

        "b" ->
            Just B

        "c" ->
            Just C

        "d" ->
            Just D

        _ ->
            Nothing
