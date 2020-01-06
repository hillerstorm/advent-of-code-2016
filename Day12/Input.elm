module Day12.Input exposing (Instruction(..), Register(..), Value(..), parsedInput, rawInput)


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
    String.lines rawInput
        |> List.filterMap parse


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
            Maybe.map2 Cpy (parseValue val) (parseRegister reg)

        [ "inc", reg ] ->
            Maybe.map Inc (parseRegister reg)

        [ "dec", reg ] ->
            Maybe.map Dec (parseRegister reg)

        [ "jnz", val, step ] ->
            Maybe.map2 Jnz (parseValue val) (String.toInt step)

        _ ->
            Nothing


parseValue : String -> Maybe Value
parseValue str =
    case parseRegister str of
        Just register ->
            Just (Reg register)

        Nothing ->
            Maybe.map Num (String.toInt str)


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
