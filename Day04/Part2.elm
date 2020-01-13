module Day04.Part2 exposing (main)

import Char exposing (fromCode, toCode)
import Day04.Input exposing (Room, parsedInput)
import Day04.Part1 exposing (validRoom)
import Html exposing (Html, div, text)


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
                    let
                        code =
                            1 + toCode chr
                    in
                    shift (steps - 1) (fromCode code)


decrypt : Room -> Room
decrypt room =
    { room
        | name = String.map (shift room.sector) room.name
    }


isStorage : Room -> Bool
isStorage room =
    room.name == "northpole object storage"


solve : List Room -> String
solve =
    List.filter validRoom
        >> List.map decrypt
        >> List.filter isStorage
        >> List.head
        >> Maybe.map (.sector >> String.fromInt)
        >> Maybe.withDefault "No result..."


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ solve parsedInput)
            ]
        ]
