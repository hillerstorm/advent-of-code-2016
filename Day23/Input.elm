module Day23.Input exposing (rawInput, parsedInput, Instruction(..), Value(..), Register(..))


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
    List.filterMap parse <| String.lines rawInput


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
            case ( parseValue val, parseRegister reg ) of
                ( Just value, Just register ) ->
                    Just <| Cpy value register

                _ ->
                    Nothing

        [ "inc", reg ] ->
            case parseRegister reg of
                Just register ->
                    Just <| Inc register

                Nothing ->
                    Nothing

        [ "dec", reg ] ->
            case parseRegister reg of
                Just register ->
                    Just <| Dec register

                Nothing ->
                    Nothing

        [ "jnz", val, step ] ->
            case ( parseValue val, parseValue step ) of
                ( Just value, Just steps ) ->
                    Just <| Jnz value steps

                _ ->
                    Nothing

        [ "tgl", reg ] ->
            case parseRegister reg of
                Just register ->
                    Just <| Tgl register

                Nothing ->
                    Nothing

        _ ->
            Nothing


parseValue : String -> Maybe Value
parseValue str =
    case ( parseRegister str, String.toInt str ) of
        ( Just register, _ ) ->
            Just <| Reg register

        ( _, Ok value ) ->
            Just <| Num value

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
