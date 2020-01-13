module Day08.Day8 exposing (main)

import Array exposing (Array)
import Day08.Input exposing (Instruction(..), parsedInput)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


main : Html msg
main =
    div []
        ([ div []
            [ text ("Part 1: " ++ (String.fromInt <| solve <| draw parsedInput))
            ]
         , div []
            [ text "Part 2: "
            ]
         ]
            ++ (draw parsedInput
                    |> Array.map Array.toList
                    |> Array.toList
                    |> List.map drawRow
               )
        )


solve : Array (Array Bool) -> Int
solve =
    Array.map (Array.filter ((==) True) >> Array.length)
        >> Array.foldl (+) 0


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
    always False
        |> Array.initialize gridWidth
        |> always
        |> Array.initialize gridHeight


gridHeight : Int
gridHeight =
    6


gridWidth : Int
gridWidth =
    50


drawRect : Int -> Int -> Array (Array Bool) -> Array (Array Bool)
drawRect width height =
    mapRectRow width height
        |> Array.indexedMap


rotateRow : Int -> Int -> Array (Array Bool) -> Array (Array Bool)
rotateRow rowIndex steps =
    mapRowColumn rowIndex steps
        |> Array.indexedMap


rotateColumn : Int -> Int -> Array (Array Bool) -> Array (Array Bool)
rotateColumn columnIndex steps result =
    Array.indexedMap (mapColumnRow columnIndex steps result) result


mapRectRow : Int -> Int -> Int -> Array Bool -> Array Bool
mapRectRow width height rowIndex =
    mapRectCell width height rowIndex
        |> Array.indexedMap


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
    Array.get (modBy gridWidth (columnIndex - steps)) row
        |> Maybe.withDefault cell


mapColumnRow : Int -> Int -> Array (Array Bool) -> Int -> Array Bool -> Array Bool
mapColumnRow columnIndex steps result rowIndex =
    mapColumnCell columnIndex steps result rowIndex
        |> Array.indexedMap


mapColumnCell : Int -> Int -> Array (Array Bool) -> Int -> Int -> Bool -> Bool
mapColumnCell columnIndex steps result rowIndex index cell =
    if index == columnIndex then
        Array.get (modBy gridHeight (rowIndex - steps)) result
            |> Maybe.andThen (Array.get index)
            |> Maybe.withDefault cell

    else
        cell


drawRow : List Bool -> Html msg
drawRow row =
    div [ style "font-family" "monospace" ]
        [ text <| String.fromList <| List.map (eitherOr '█' '░') row
        ]


eitherOr : a -> a -> Bool -> a
eitherOr true false x =
    if x == True then
        true

    else
        false
