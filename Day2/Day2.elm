module Day2.Day2 exposing (main)

import Day2.Input exposing (Direction(..), parsedInput)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task exposing (..)
import Process exposing (sleep)
import Time exposing (millisecond)


type alias Model =
    { rows : List (List Direction)
    , currentPart : Part
    , currentRow : Maybe (List Direction)
    , currentGrid : String
    , lastPosition : ( Int, Int )
    , firstPart : String
    , secondPart : String
    , totalMoves : Int
    }


firstGrid : String
firstGrid =
    "....."
        ++ ".123."
        ++ ".456."
        ++ ".789."
        ++ "....."


secondGrid : String
secondGrid =
    "......."
        ++ "...1..."
        ++ "..234.."
        ++ ".56789."
        ++ "..ABC.."
        ++ "...D..."
        ++ "......."


type Part
    = One
    | Two


type Msg
    = NextRow
    | Move
    | NextPart


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    { rows = parsedInput
    , currentPart = One
    , currentRow = Nothing
    , currentGrid = firstGrid
    , lastPosition = ( 2, 2 )
    , firstPart = ""
    , secondPart = ""
    , totalMoves = List.sum <| List.map List.length parsedInput
    }
        ! [ trigger 0 NextRow ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextRow ->
            case List.head model.rows of
                (Just _) as row ->
                    let
                        rows =
                            case List.tail model.rows of
                                Just r ->
                                    r

                                Nothing ->
                                    []
                    in
                        { model | currentRow = row, rows = rows } ! [ trigger 2 Move ]

                Nothing ->
                    { model | currentRow = Nothing } ! [ trigger 300 NextPart ]

        Move ->
            case model.currentRow of
                Just (x :: xs) ->
                    move model x xs

                _ ->
                    let
                        idx =
                            getIdx model.lastPosition model.currentGrid

                        chr =
                            String.slice idx (idx + 1) model.currentGrid
                    in
                        case model.currentPart of
                            One ->
                                { model | firstPart = model.firstPart ++ chr } ! [ trigger 300 NextRow ]

                            Two ->
                                { model | secondPart = model.secondPart ++ chr } ! [ trigger 300 NextRow ]

        NextPart ->
            case model.currentPart of
                One ->
                    { model
                        | rows = parsedInput
                        , currentPart = Two
                        , currentRow = Nothing
                        , currentGrid = secondGrid
                        , lastPosition = ( 1, 3 )
                    }
                        ! [ trigger 0 NextRow ]

                Two ->
                    model ! []


getIdx : ( Int, Int ) -> String -> Int
getIdx ( x, y ) grid =
    x + y * getWidth grid


getWidth : String -> Int
getWidth =
    floor << sqrt << toFloat << String.length


trigger : Float -> msg -> Cmd msg
trigger delay msg =
    if delay > 0 then
        Process.sleep (delay * millisecond) |> Task.perform (\_ -> msg)
    else
        Task.perform identity <| Task.succeed msg


move : Model -> Direction -> List Direction -> ( Model, Cmd Msg )
move ({ lastPosition, currentGrid } as model) direction currentRow =
    let
        ( x, y ) =
            lastPosition

        nextPosition =
            case direction of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

        idx =
            getIdx nextPosition currentGrid
    in
        case String.slice idx (idx + 1) currentGrid of
            "." ->
                { model | currentRow = Just currentRow } ! [ trigger 0 Move ]

            chr ->
                { model
                    | currentRow = Just currentRow
                    , lastPosition = nextPosition
                }
                    ! [ trigger 2 Move ]


view : Model -> Html msg
view model =
    div []
        [ div [] [ Html.text <| "Part 1: " ++ model.firstPart ]
        , div [] [ Html.text <| "Part 2: " ++ model.secondPart ]
        , drawGrid model.lastPosition model.currentGrid
        , div [] []
        , drawProgress model.totalMoves model.rows model.currentRow
        ]


drawProgress : Int -> List (List Direction) -> Maybe (List Direction) -> Html msg
drawProgress totalMoves rows currentRow =
    let
        rowCount =
            List.sum <| List.map List.length rows

        totalCount =
            case currentRow of
                Just row ->
                    rowCount + (List.length row)

                Nothing ->
                    rowCount
    in
        if totalCount == 0 then
            div [] []
        else
            let
                progressWidth =
                    toString <| floor <| (toFloat totalCount / toFloat totalMoves) * 300
            in
                svg [ width "300", height "10" ] [ rect [ x "0", y "0", height "10", width progressWidth ] [] ]


drawGrid : ( Int, Int ) -> String -> Html msg
drawGrid ( x, y ) currentGrid =
    let
        gridWidth =
            getWidth currentGrid

        cellSize =
            300 // gridWidth

        padding =
            cellSize // 2

        numbers =
            currentGrid
                |> String.toList
                |> List.indexedMap (,)
                |> List.filterMap isValid
                |> List.map (toText cellSize gridWidth)

        active =
            Svg.circle
                [ cx <| toString <| (x * cellSize) + padding
                , cy <| toString <| (y * cellSize) + padding
                , r <| toString <| padding
                , fill "green"
                ]
                []
    in
        svg [ width "300", height "300" ] <| active :: numbers


isValid : ( Int, Char ) -> Maybe ( Int, String )
isValid ( idx, chr ) =
    case chr of
        '.' ->
            Nothing

        _ ->
            Just ( idx, String.fromChar chr )


toText : Int -> Int -> ( Int, String ) -> Svg msg
toText cellSize gridWidth ( idx, chr ) =
    let
        xPadding =
            floor <| (toFloat cellSize) * 0.2

        yPadding =
            floor <| (toFloat cellSize) * 0.8
    in
        Svg.text_
            [ x <| toString <| ((idx % gridWidth) * cellSize) + xPadding
            , y <| toString <| ((idx // gridWidth) * cellSize) + yPadding
            , fontSize <| toString cellSize
            , fontFamily "Courier New"
            ]
            [ Html.text chr ]
