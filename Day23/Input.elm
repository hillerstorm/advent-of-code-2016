module Day23.Input exposing (Instruction(..), Register(..), Value(..), parsedInput)


rawInput : String
rawInput =
    """cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2
tgl c
cpy -16 c
jnz 1 c
cpy 75 c
jnz 88 d
inc a
inc d
jnz d -2
inc c
jnz c -5"""


parsedInput : List Instruction
parsedInput =
    String.lines rawInput
        |> List.filterMap parse


type Instruction
    = Cpy Value Register
    | Inc Register
    | IncBy Register Register
    | Dec Register
    | Jnz Value Value
    | Tgl Register
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

        [ "tgl", reg ] ->
            Maybe.map Tgl (parseRegister reg)

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
