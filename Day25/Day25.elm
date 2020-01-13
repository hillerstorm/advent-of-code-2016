module Day25.Day25 exposing (main)

import Array exposing (Array)
import Day25.Input exposing (Instruction(..), Register(..), Value(..), parsedInput)
import Html exposing (Html, div, text)


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (String.fromInt <| findAnswer 0)) ]
        ]


optimizedInitialInstructions : Array Instruction
optimizedInitialInstructions =
    optimize (Array.fromList parsedInput)


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
findIncByPattern indexed result =
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
                                    findIncByPattern nextIndexed (Array.set i (IncBy x y) result)

                                else
                                    findIncByPattern nextIndexed result

                            Num _ ->
                                findIncByPattern nextIndexed result

                    else
                        findIncByPattern nextIndexed result

                _ ->
                    findIncByPattern nextIndexed result

        _ ->
            result


findMulPattern : List ( Int, Instruction ) -> Array Instruction -> Array Instruction
findMulPattern indexed result =
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
                                    findMulPattern nextIndexed (Array.set i (Mul z k x j) result)

                                else
                                    findMulPattern nextIndexed result

                            Num _ ->
                                findMulPattern nextIndexed result

                    else
                        findMulPattern nextIndexed result

                _ ->
                    findMulPattern nextIndexed result

        _ ->
            result


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
    solve a 0 optimizedInitialInstructions (initialRegisters a)


solve : Int -> Int -> Array Instruction -> Registers -> Int
solve initialA index optimized registers =
    if index == -1 then
        case registers.out of
            "010101010101" ->
                initialA

            _ ->
                findAnswer (initialA + 1)

    else
        case Array.get index optimized of
            Just (Cpy value register) ->
                let
                    newRegisters =
                        registers
                            |> copyValue value register
                in
                solve initialA (index + 1) optimized newRegisters

            Just (Inc register) ->
                let
                    newRegisters =
                        registers
                            |> increment (Num 1) register
                in
                solve initialA (index + 1) optimized newRegisters

            Just (IncBy register value) ->
                let
                    newRegisters =
                        registers
                            |> increment (Reg value) register
                            |> reset value
                in
                solve initialA (index + 2) optimized newRegisters

            Just (Mul register val1 val2 val3) ->
                let
                    newRegisters =
                        registers
                            |> multiply val1 val2 register
                            |> reset val1
                            |> reset val3
                in
                solve initialA (index + 5) optimized newRegisters

            Just (Dec register) ->
                let
                    newRegisters =
                        registers
                            |> decrement register
                in
                solve initialA (index + 1) optimized newRegisters

            Just (Jnz value steps) ->
                let
                    newIndex =
                        index + jump value steps registers
                in
                solve initialA newIndex optimized registers

            Just (Out value) ->
                let
                    newRegisters =
                        registers
                            |> out value
                in
                solve initialA (index + 1) optimized newRegisters

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
