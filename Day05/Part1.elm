module Day05.Part1 exposing (main)

import Html exposing (Html, div, text)
import MD5 exposing (hex)


input : String
input =
    "uqwqemis"


nextChar : Int -> ( String, Int )
nextChar index =
    let
        hash =
            hex (input ++ String.fromInt index)

        nextIndex =
            index + 1
    in
    if String.startsWith "00000" hash then
        ( String.slice 5 6 hash, nextIndex )

    else
        nextChar nextIndex


solve : String -> Int -> String
solve password index =
    if String.length password == 8 then
        password

    else
        let
            ( chr, nextIndex ) =
                nextChar index
        in
        solve (password ++ chr) nextIndex


main : Html msg
main =
    div []
        [ div []
            [ text ("Input: " ++ input)
            ]
        , div []
            [ text ("Result: " ++ solve "" 0)
            ]
        ]



-- 1a3099aa
