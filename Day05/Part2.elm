module Day05.Part2 exposing (main)

import Html exposing (Html, div, text)
import MD5 exposing (hex)


input : String
input =
    "uqwqemis"


nextChar : Int -> ( String, Int, Int )
nextChar index =
    let
        hash =
            hex (input ++ String.fromInt index)

        nextIndex =
            index + 1
    in
    if String.startsWith "00000" hash then
        case String.toInt <| String.slice 5 6 hash of
            Just i ->
                if i >= String.length input then
                    nextChar nextIndex

                else
                    ( String.slice 6 7 hash, i, nextIndex )

            Nothing ->
                nextChar nextIndex

    else
        nextChar nextIndex


solve : String -> Int -> String
solve password index =
    if not <| String.contains "-" password then
        password

    else
        let
            ( chr, pwdIndex, nextIndex ) =
                nextChar index

            firstPart =
                String.slice 0 pwdIndex password

            currentChar =
                String.slice pwdIndex (pwdIndex + 1) password

            lastPart =
                String.slice (pwdIndex + 1) (String.length password) password

            newPassword =
                let
                    middlePart =
                        if currentChar == "-" then
                            chr

                        else
                            currentChar
                in
                firstPart ++ middlePart ++ lastPart
        in
        solve newPassword nextIndex


main : Html msg
main =
    div []
        [ div []
            [ text ("Result: " ++ solve "--------" 0)
            ]
        ]



-- 694190cd
