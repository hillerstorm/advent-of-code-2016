module Day23.Day23 exposing (main)

import Array exposing (..)
import Html exposing (..)
import Day23.Input exposing (Instruction(..), Register(..), Value(..), parsedInput, rawInput)


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Part 1: " ++ (toString <| solve 0 initialInstructions optimizedInitialInstructions ( 7, 0, 0, 0 ))) ]
        , div [] [ text ("Part 2: " ++ (toString <| solve 0 initialInstructions optimizedInitialInstructions ( 12, 0, 0, 0 ))) ]
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
findIncByPattern indexed =
    case indexed of
        ( i, a ) :: b :: c :: xs ->
            let
                nextIndexed =
                    b :: c :: xs
            in
                case ( a, Tuple.second b, Tuple.second c ) of
                    ( Inc x, Dec y, Jnz z (Num -2) ) ->
                        case z of
                            Reg r ->
                                if y == r then
                                    findIncByPattern nextIndexed << Array.set i (IncBy x y)
                                else
                                    findIncByPattern nextIndexed

                            Num _ ->
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
                case ( a, Tuple.second b, Tuple.second e, Tuple.second f ) of
                    ( Cpy x y, IncBy z j, Dec k, Jnz l (Num -5) ) ->
                        case l of
                            Reg r ->
                                if y == j && k == r then
                                    findMulPattern nextIndexed << Array.set i (Mul z k x j)
                                else
                                    findMulPattern nextIndexed

                            Num _ ->
                                findMulPattern nextIndexed

                    _ ->
                        findMulPattern nextIndexed

        _ ->
            identity


solve : Int -> Array Instruction -> Array Instruction -> Registers -> Int
solve index original optimized registers =
    if index == -1 then
        let
            ( a, _, _, _ ) =
                registers
        in
            a
    else
        case Array.get index optimized of
            Just (Cpy value register) ->
                registers
                    |> copyValue value register
                    |> solve (index + 1) original optimized

            Just (Inc register) ->
                registers
                    |> increment (Num 1) register
                    |> solve (index + 1) original optimized

            Just (IncBy register value) ->
                registers
                    |> increment (Reg value) register
                    |> reset value
                    |> solve (index + 2) original optimized

            Just (Mul register val1 val2 val3) ->
                registers
                    |> multiply val1 val2 register
                    |> reset val1
                    |> reset val3
                    |> solve (index + 5) original optimized

            Just (Dec register) ->
                registers
                    |> decrement register
                    |> solve (index + 1) original optimized

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
    ( Int, Int, Int, Int )


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
modify f register ( a, b, c, d ) =
    case register of
        A ->
            ( f a, b, c, d )

        B ->
            ( a, f b, c, d )

        C ->
            ( a, b, f c, d )

        D ->
            ( a, b, c, f d )


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
    let
        ( a, b, c, d ) =
            registers
    in
        case register of
            A ->
                a

            B ->
                b

            C ->
                c

            D ->
                d


toggle : Register -> Int -> Array Instruction -> Registers -> Array Instruction
toggle register index instructions registers =
    let
        toggleIndex =
            (index + getRegister register registers)

        newInstruction =
            case Array.get toggleIndex instructions of
                Just (Inc register) ->
                    Just <| Dec register

                Just (Dec register) ->
                    Just <| Inc register

                Just (Tgl register) ->
                    Just <| Inc register

                Just (Cpy value register) ->
                    Just <| Jnz value <| Reg register

                Just (Jnz value steps) ->
                    case steps of
                        Reg r ->
                            Just <| Cpy value r

                        Num _ ->
                            Just noop

                (Just _) as other ->
                    other

                Nothing ->
                    Nothing
    in
        case newInstruction of
            Just instruction ->
                Array.set toggleIndex instruction instructions

            Nothing ->
                instructions


noop : Instruction
noop =
    Jnz (Num 0) <| Num 0
