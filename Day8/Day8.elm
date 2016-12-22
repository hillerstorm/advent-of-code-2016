module Day8.Day8 exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Day8.Input exposing (rawInput, parsedInput, Instruction(..))


draw : List Instruction -> Array (Array Pixel)
draw =
    List.foldl execute baseArray


solve : Array (Array Pixel) -> Int
solve pixels =
    pixels
        |> Array.map (Array.length << Array.filter ((==) On))
        |> Array.foldl (+) 0


execute : Instruction -> Array (Array Pixel) -> Array (Array Pixel)
execute instruction result =
    case instruction of
        Rect width height ->
            drawRect width height result

        Row row steps ->
            rotateRow row steps result

        Column col steps ->
            rotateColumn col steps result


type Pixel
    = On
    | Off


drawRect : Int -> Int -> Array (Array Pixel) -> Array (Array Pixel)
drawRect width height =
    Array.indexedMap (mapRectRow width height)


mapRectRow : Int -> Int -> Int -> Array Pixel -> Array Pixel
mapRectRow width height rowIndex =
    Array.indexedMap (mapRectCell width height rowIndex)


mapRectCell : Int -> Int -> Int -> Int -> Pixel -> Pixel
mapRectCell width height rowIndex columnIndex cell =
    if rowIndex < height && columnIndex < width then
        On
    else
        cell


rotateRow : Int -> Int -> Array (Array Pixel) -> Array (Array Pixel)
rotateRow rowIndex steps =
    Array.indexedMap (mapRowColumn rowIndex steps)


mapRowColumn : Int -> Int -> Int -> Array Pixel -> Array Pixel
mapRowColumn rowIndex steps index row =
    if index == rowIndex then
        Array.indexedMap (mapRowCell row steps) row
    else
        row


mapRowCell : Array Pixel -> Int -> Int -> Pixel -> Pixel
mapRowCell row steps columnIndex cell =
    case Array.get ((columnIndex - steps) % gridWidth) row of
        Just value ->
            value

        _ ->
            cell


rotateColumn : Int -> Int -> Array (Array Pixel) -> Array (Array Pixel)
rotateColumn columnIndex steps result =
    Array.indexedMap (mapColumnRow columnIndex steps result) result


mapColumnRow : Int -> Int -> Array (Array Pixel) -> Int -> Array Pixel -> Array Pixel
mapColumnRow columnIndex steps result rowIndex =
    Array.indexedMap (mapColumnCell columnIndex steps result rowIndex)


mapColumnCell : Int -> Int -> Array (Array Pixel) -> Int -> Int -> Pixel -> Pixel
mapColumnCell columnIndex steps result rowIndex index cell =
    if index == columnIndex then
        case Array.get ((rowIndex - steps) % gridHeight) result of
            Just row ->
                case Array.get index row of
                    Just value ->
                        value

                    _ ->
                        cell

            _ ->
                cell
    else
        cell


baseArray : Array (Array Pixel)
baseArray =
    Array.initialize gridHeight <| always <| Array.initialize gridWidth <| always Off


gridHeight : Int
gridHeight =
    6


gridWidth : Int
gridWidth =
    50


main : Html msg
main =
    div []
        ([ div [] [ text ("Input: " ++ (toString <| rawInput)) ]
         , div [] [ text ("Part 1: " ++ (toString <| solve <| draw parsedInput)) ]
         , div [] [ text "Part 2: " ]
         ]
            ++ (List.map drawRow (Array.toList <| Array.map Array.toList <| draw parsedInput))
        )


drawRow : List Pixel -> Html msg
drawRow row =
    div [ style [ ( "font-family", "monospace" ) ] ]
        [ text <|
            String.fromList <|
                List.map
                    (\x ->
                        case x of
                            On ->
                                '█'

                            Off ->
                                '░'
                    )
                    row
        ]
