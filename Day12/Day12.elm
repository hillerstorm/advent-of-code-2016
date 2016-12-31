module Day12.Day12 exposing (main)

import Html exposing (..)
import Array exposing (..)
import Day12.Input exposing (rawInput, parsedInput, Instruction(..), Value(..), Register(..))


copyValue : Value -> Registers -> Register -> Registers
copyValue value registers =
    modify
        (\_ ->
            let
                ( a, b, c, d ) =
                    registers
            in
                case value of
                    Num n ->
                        n

                    Reg A ->
                        a

                    Reg B ->
                        b

                    Reg C ->
                        c

                    Reg D ->
                        d
        )
        registers


type alias Registers =
    ( Int, Int, Int, Int )


increment : Registers -> Register -> Registers
increment =
    modify (\x -> x + 1)


decrement : Registers -> Register -> Registers
decrement =
    modify (\x -> x - 1)


modify : (Int -> Int) -> Registers -> Register -> Registers
modify f ( a, b, c, d ) register =
    case register of
        A ->
            ( f a, b, c, d )

        B ->
            ( a, f b, c, d )

        C ->
            ( a, b, f c, d )

        D ->
            ( a, b, c, f d )


jump : Registers -> Value -> Int -> Int -> Int
jump registers value steps index =
    let
        val =
            case value of
                Num x ->
                    x

                Reg r ->
                    let
                        ( a, b, c, d ) =
                            registers
                    in
                        case r of
                            A ->
                                a

                            B ->
                                b

                            C ->
                                c

                            D ->
                                d
    in
        if val == 0 then
            1
        else
            steps


solve : Registers -> Int -> Array Instruction -> Int
solve registers index instructions =
    if index == -1 then
        let
            ( a, _, _, _ ) =
                registers
        in
            a
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
                        ( registers, index + jump registers value steps index )

                    Nothing ->
                        ( registers, -1 )
        in
            solve newRegisters newIndex instructions


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Part 1: " ++ (toString <| solve ( 0, 0, 0, 0 ) 0 <| Array.fromList parsedInput)) ]
        , div [] [ text ("Part 2: " ++ (toString <| solve ( 0, 0, 1, 0 ) 0 <| Array.fromList parsedInput)) ]
        ]
