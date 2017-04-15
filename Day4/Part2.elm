module Day4.Part2 exposing (main)

import Html exposing (..)
import Char exposing (fromCode, toCode)
import Day4.Part1 exposing (validRoom)
import Day4.Input exposing (rawInput, parsedInput, Room)


shift : Int -> Char -> Char
shift steps chr =
    case steps of
        0 ->
            chr

        _ ->
            case chr of
                '-' ->
                    ' '

                'z' ->
                    shift (steps - 1) 'a'

                _ ->
                    shift (steps - 1) <| fromCode <| (+) 1 <| toCode chr


decrypt : Room -> Room
decrypt room =
    { room | name = String.map (shift room.sector) room.name }


isStorage : Room -> Bool
isStorage { name } =
    name == "northpole object storage"


solve : List Room -> Maybe Int
solve =
    List.filter validRoom
        >> List.map decrypt
        >> List.filter isStorage
        >> List.head
        >> Maybe.map .sector


main : Html msg
main =
    div []
        [ div [] [ text ("Input: " ++ rawInput) ]
        , div [] [ text ("Result: " ++ (toString <| solve parsedInput)) ]
        ]
