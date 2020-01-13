module Day12.Day12 exposing (main)

import Array exposing (Array)
import Day12.Input exposing (Instruction(..), Register(..), Value(..), parsedInput)
import Html exposing (Html, div, text)


copyValue : Value -> Registers -> Register -> Registers
copyValue value registers =
    modify
        (\_ -> getValue value registers)
        registers


type alias Registers =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


increment : Registers -> Register -> Registers
increment =
    modify (\x -> x + 1)


decrement : Registers -> Register -> Registers
decrement =
    modify (\x -> x - 1)


modify : (Int -> Int) -> Registers -> Register -> Registers
modify f registers register =
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


getValue : Value -> Registers -> Int
getValue value registers =
    case value of
        Num n ->
            n

        Reg A ->
            registers.a

        Reg B ->
            registers.b

        Reg C ->
            registers.c

        Reg D ->
            registers.d


jump : Registers -> Value -> Int -> Int
jump registers value steps =
    let
        val =
            getValue value registers
    in
    if val == 0 then
        1

    else
        steps


solve : Registers -> Int -> Array Instruction -> Int
solve registers index instructions =
    if index == -1 then
        registers.a

    else
        let
            ( newRegisters, newIndex ) =
                case Array.get index instructions of
                    Just (Cpy value register) ->
                        ( copyValue value registers register, index + 1 )

                    Just (Inc register) ->
                        ( increment registers register, index + 1 )

                    Just (Dec register) ->
                        ( decrement registers register, index + 1 )

                    Just (Jnz value steps) ->
                        ( registers, index + jump registers value steps )

                    Nothing ->
                        ( registers, -1 )
        in
        solve newRegisters newIndex instructions


initialPart1 : Registers
initialPart1 =
    { a = 0
    , b = 0
    , c = 0
    , d = 0
    }


initialPart2 : Registers
initialPart2 =
    { a = 0
    , b = 0
    , c = 1
    , d = 0
    }


main : Html msg
main =
    div []
        [ div [] [ text ("Part 1: " ++ (String.fromInt <| solve initialPart1 0 <| Array.fromList parsedInput)) ]
        , div [] [ text ("Part 2: " ++ (String.fromInt <| solve initialPart2 0 <| Array.fromList parsedInput)) ]
        ]
