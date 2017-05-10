module Day25.Input exposing (rawInput, parsedInput, Instruction(..), Value(..), Register(..))


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
    List.filterMap parse <| List.reverse <| List.drop 1 <| List.reverse <| String.lines rawInput


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

        [ "out", reg ] ->
            case parseValue reg of
                Just value ->
                    Just <| Out value

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
