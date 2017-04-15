module Day8.Day8 exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Day8.Input exposing (rawInput, parsedInput, Instruction(..))


main : Html msg
main =
    div []
        ([ div [] [ text ("Input: " ++ (toString <| rawInput)) ]
         , div [] [ text ("Part 1: " ++ (toString <| solve <| draw parsedInput)) ]
         , div [] [ text "Part 2: " ]
         ]
            ++ (List.map drawRow <| Array.toList <| Array.map Array.toList <| draw parsedInput)
        )


solve : Array (Array Bool) -> Int
solve =
    Array.foldl (+) 0 << Array.map (Array.length << Array.filter ((==) True))


draw : List Instruction -> Array (Array Bool)
draw =
    List.foldl execute baseArray


execute : Instruction -> Array (Array Bool) -> Array (Array Bool)
execute instruction =
    case instruction of
        Rect width height ->
            drawRect width height

        Row row steps ->
            rotateRow row steps

        Column col steps ->
            rotateColumn col steps


baseArray : Array (Array Bool)
baseArray =
    Array.initialize gridHeight <| always <| Array.initialize gridWidth <| always False


gridHeight : Int
gridHeight =
    6


gridWidth : Int
gridWidth =
    50


drawRect : Int -> Int -> Array (Array Bool) -> Array (Array Bool)
drawRect width height =
    Array.indexedMap <| mapRectRow width height


rotateRow : Int -> Int -> Array (Array Bool) -> Array (Array Bool)
rotateRow rowIndex steps =
    Array.indexedMap <| mapRowColumn rowIndex steps


rotateColumn : Int -> Int -> Array (Array Bool) -> Array (Array Bool)
rotateColumn columnIndex steps result =
    Array.indexedMap (mapColumnRow columnIndex steps result) result


mapRectRow : Int -> Int -> Int -> Array Bool -> Array Bool
mapRectRow width height rowIndex =
    Array.indexedMap <| mapRectCell width height rowIndex


mapRectCell : Int -> Int -> Int -> Int -> Bool -> Bool
mapRectCell width height rowIndex columnIndex cell =
    rowIndex < height && columnIndex < width || cell


mapRowColumn : Int -> Int -> Int -> Array Bool -> Array Bool
mapRowColumn rowIndex steps index row =
    if index == rowIndex then
        Array.indexedMap (mapRowCell row steps) row
    else
        row


mapRowCell : Array Bool -> Int -> Int -> Bool -> Bool
mapRowCell row steps columnIndex cell =
    Array.get ((columnIndex - steps) % gridWidth) row
        |> Maybe.withDefault cell


mapColumnRow : Int -> Int -> Array (Array Bool) -> Int -> Array Bool -> Array Bool
mapColumnRow columnIndex steps result rowIndex =
    Array.indexedMap <| mapColumnCell columnIndex steps result rowIndex


mapColumnCell : Int -> Int -> Array (Array Bool) -> Int -> Int -> Bool -> Bool
mapColumnCell columnIndex steps result rowIndex index cell =
    if index == columnIndex then
        Array.get ((rowIndex - steps) % gridHeight) result
            |> Maybe.andThen (Array.get index)
            |> Maybe.withDefault cell
    else
        cell


drawRow : List Bool -> Html msg
drawRow row =
    div [ style [ ( "font-family", "monospace" ) ] ]
        [ text <|
            String.fromList <|
                List.map (eitherOr '█' '░') row
        ]


eitherOr : a -> a -> Bool -> a
eitherOr true false x =
    if x == True then
        true
    else
        false
