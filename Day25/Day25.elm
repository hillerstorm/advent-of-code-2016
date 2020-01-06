module Day25.Day25 exposing (main)

import Array exposing (Array)
import Day25.Input exposing (Instruction(..), Register(..), Value(..), parsedInput, rawInput)
import Html exposing (Html, div, text)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Part 1: " ++ (String.fromInt <| findAnswer 0)) ]
        ]


optimizedInitialInstructions : Array Instruction
optimizedInitialInstructions =
    optimize <| Array.fromList parsedInput


optimize : Array Instruction -> Array Instruction
optimize instructions =
    let
        firstPass =
            instructions
                |> findIncByPattern (Array.toIndexedList instructions)

        indexed =
            Array.toIndexedList firstPass
    in
    findMulPattern indexed firstPass


findIncByPattern : List ( Int, Instruction ) -> Array Instruction -> Array Instruction
findIncByPattern indexed =
    case indexed of
        ( i, a ) :: b :: c :: xs ->
            let
                nextIndexed =
                    b :: c :: xs
            in
            case ( a, Tuple.second b, Tuple.second c ) of
                ( Inc x, Dec y, Jnz z (Num num) ) ->
                    if num == -2 then
                        case z of
                            Reg r ->
                                if y == r then
                                    Array.set i (IncBy x y) >> findIncByPattern nextIndexed

                                else
                                    findIncByPattern nextIndexed

                            Num _ ->
                                findIncByPattern nextIndexed

                    else
                        findIncByPattern nextIndexed

                _ ->
                    findIncByPattern nextIndexed

        _ ->
            identity


findMulPattern : List ( Int, Instruction ) -> Array Instruction -> Array Instruction
findMulPattern indexed =
    case indexed of
        ( i, a ) :: b :: c :: d :: e :: f :: xs ->
            let
                nextIndexed =
                    b :: c :: d :: e :: f :: xs
            in
            case [ a, Tuple.second b, Tuple.second e, Tuple.second f ] of
                [ Cpy x y, IncBy z j, Dec k, Jnz l (Num num) ] ->
                    if num == -5 then
                        case l of
                            Reg r ->
                                if y == j && k == r then
                                    Array.set i (Mul z k x j) >> findMulPattern nextIndexed

                                else
                                    findMulPattern nextIndexed

                            Num _ ->
                                findMulPattern nextIndexed

                    else
                        findMulPattern nextIndexed

                _ ->
                    findMulPattern nextIndexed

        _ ->
            identity


initialRegisters : Int -> Registers
initialRegisters a =
    { a = a
    , b = 0
    , c = 0
    , d = 0
    , out = ""
    }


findAnswer : Int -> Int
findAnswer a =
    solve a 0 optimizedInitialInstructions <| initialRegisters a


solve : Int -> Int -> Array Instruction -> Registers -> Int
solve initialA index optimized registers =
    if index == -1 then
        case registers.out of
            "010101010101" ->
                initialA

            _ ->
                findAnswer <| initialA + 1

    else
        case Array.get index optimized of
            Just (Cpy value register) ->
                registers
                    |> copyValue value register
                    |> solve initialA (index + 1) optimized

            Just (Inc register) ->
                registers
                    |> increment (Num 1) register
                    |> solve initialA (index + 1) optimized

            Just (IncBy register value) ->
                registers
                    |> increment (Reg value) register
                    |> reset value
                    |> solve initialA (index + 2) optimized

            Just (Mul register val1 val2 val3) ->
                registers
                    |> multiply val1 val2 register
                    |> reset val1
                    |> reset val3
                    |> solve initialA (index + 5) optimized

            Just (Dec register) ->
                registers
                    |> decrement register
                    |> solve initialA (index + 1) optimized

            Just (Jnz value steps) ->
                let
                    newIndex =
                        index + jump value steps registers
                in
                solve initialA newIndex optimized registers

            Just (Out value) ->
                registers
                    |> out value
                    |> solve initialA (index + 1) optimized

            Nothing ->
                solve initialA -1 optimized registers


type alias Registers =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , out : String
    }


increment : Value -> Register -> Registers -> Registers
increment value register registers =
    modify (\x -> x + getValue value registers) register registers


decrement : Register -> Registers -> Registers
decrement =
    modify (\x -> x - 1)


reset : Register -> Registers -> Registers
reset =
    modify (always 0)


multiply : Register -> Value -> Register -> Registers -> Registers
multiply reg val register registers =
    let
        firstVal =
            getRegister reg registers

        secondVal =
            getValue val registers
    in
    modify (\x -> x + (firstVal * secondVal)) register registers


copyValue : Value -> Register -> Registers -> Registers
copyValue value register registers =
    let
        val =
            case value of
                Num n ->
                    n

                Reg r ->
                    getRegister r registers
    in
    modify (always val) register registers


modify : (Int -> Int) -> Register -> Registers -> Registers
modify f register registers =
    case register of
        A ->
            { registers
                | a = f registers.a
            }

        B ->
            { registers
                | b = f registers.b
            }

        C ->
            { registers
                | c = f registers.c
            }

        D ->
            { registers
                | d = f registers.d
            }


jump : Value -> Value -> Registers -> Int
jump value steps registers =
    let
        val =
            getValue value registers
    in
    if val == 0 then
        1

    else
        getValue steps registers


out : Value -> Registers -> Registers
out value registers =
    let
        val =
            String.fromInt <| getValue value registers
    in
    { registers
        | out = registers.out ++ val
    }


getValue : Value -> Registers -> Int
getValue value =
    case value of
        Num x ->
            always x

        Reg r ->
            getRegister r


getRegister : Register -> Registers -> Int
getRegister register =
    case register of
        A ->
            .a

        B ->
            .b

        C ->
            .c

        D ->
            .d
