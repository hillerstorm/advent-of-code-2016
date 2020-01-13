module Day25.Input exposing (Instruction(..), Register(..), Value(..), parsedInput)


rawInput : String
rawInput =
    """cpy a d
cpy 4 c
cpy 643 b
inc d
dec b
jnz b -2
dec c
jnz c -5
cpy d a
jnz 0 0
cpy a b
cpy 0 a
cpy 2 c
jnz b 2
jnz 1 6
dec b
dec c
jnz c -4
inc a
jnz 1 -7
cpy 2 b
jnz c 2
jnz 1 4
dec b
dec c
jnz 1 -4
jnz 0 0
out b
jnz a -19
jnz 1 -21"""


parsedInput : List Instruction
parsedInput =
    String.lines rawInput
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> List.filterMap parse


type Instruction
    = Cpy Value Register
    | Inc Register
    | IncBy Register Register
    | Dec Register
    | Jnz Value Value
    | Out Value
    | Mul Register Register Value Register


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
            Maybe.map2 Jnz (parseValue val) (parseValue step)

        [ "out", reg ] ->
            Maybe.map Out (parseValue reg)

        _ ->
            Nothing


parseValue : String -> Maybe Value
parseValue str =
    case parseRegister str of
        Just register ->
            Just <| Reg register

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
