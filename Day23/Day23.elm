module Day23.Day23 exposing (main)

import Array exposing (Array)
import Day23.Input exposing (Instruction(..), Register(..), Value(..), parsedInput)
import Html exposing (Html, div, text)


initialPart1 : Registers
initialPart1 =
    { a = 7
    , b = 0
    , c = 0
    , d = 0
    }


initialPart2 : Registers
initialPart2 =
    { a = 12
    , b = 0
    , c = 0
    , d = 0
    }


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (String.fromInt <| solve 0 initialInstructions optimizedInitialInstructions initialPart1)) ]
        , div [] [ text ("Part 2: " ++ (String.fromInt <| solve 0 initialInstructions optimizedInitialInstructions initialPart2)) ]
        ]


initialInstructions : Array Instruction
initialInstructions =
    Array.fromList parsedInput


optimizedInitialInstructions : Array Instruction
optimizedInitialInstructions =
    optimize initialInstructions


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


solve : Int -> Array Instruction -> Array Instruction -> Registers -> Int
solve index original optimized registers =
    if index == -1 then
        registers.a

    else
        case Array.get index optimized of
            Just (Cpy value register) ->
                let
                    newRegisters =
                        registers
                            |> copyValue value register
                in
                solve (index + 1) original optimized newRegisters

            Just (Inc register) ->
                let
                    newRegisters =
                        registers
                            |> increment (Num 1) register
                in
                solve (index + 1) original optimized newRegisters

            Just (IncBy register value) ->
                let
                    newRegisters =
                        registers
                            |> increment (Reg value) register
                            |> reset value
                in
                solve (index + 2) original optimized newRegisters

            Just (Mul register val1 val2 val3) ->
                let
                    newRegisters =
                        registers
                            |> multiply val1 val2 register
                            |> reset val1
                            |> reset val3
                in
                solve (index + 5) original optimized newRegisters

            Just (Dec register) ->
                let
                    newRegisters =
                        registers
                            |> decrement register
                in
                solve (index + 1) original optimized newRegisters

            Just (Jnz value steps) ->
                let
                    newIndex =
                        index + jump value steps registers
                in
                solve newIndex original optimized registers

            Just (Tgl register) ->
                let
                    newOriginal =
                        registers
                            |> toggle register index original

                    newOptimized =
                        optimize newOriginal

                    newIndex =
                        index + 1
                in
                solve newIndex newOriginal newOptimized registers

            _ ->
                solve -1 original optimized registers


type alias Registers =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
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


getValue : Value -> Registers -> Int
getValue value =
    case value of
        Num x ->
            always x

        Reg r ->
            getRegister r


getRegister : Register -> Registers -> Int
getRegister register registers =
    case register of
        A ->
            registers.a

        B ->
            registers.b

        C ->
            registers.c

        D ->
            registers.d


toggle : Register -> Int -> Array Instruction -> Registers -> Array Instruction
toggle register index instructions registers =
    let
        toggleIndex =
            index + getRegister register registers

        newInstruction =
            Maybe.map
                (\instruction ->
                    case instruction of
                        Inc reg ->
                            Dec reg

                        Dec reg ->
                            Inc reg

                        Tgl reg ->
                            Inc reg

                        Cpy value reg ->
                            Jnz value <| Reg reg

                        Jnz value steps ->
                            case steps of
                                Reg r ->
                                    Cpy value r

                                Num _ ->
                                    noop

                        other ->
                            other
                )
                (Array.get toggleIndex instructions)
    in
    case newInstruction of
        Just instruction ->
            Array.set toggleIndex instruction instructions

        Nothing ->
            instructions


noop : Instruction
noop =
    Jnz (Num 0) <| Num 0
