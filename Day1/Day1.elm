module Day1.Day1 exposing (main)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (mapFirst, mapSecond)
import Day1.Input exposing (rawInput, Move(..), parsedInput)
import Task exposing (..)
import Process exposing (sleep)
import Time exposing (millisecond)


type alias Model =
    { moves : List Move
    , positions : ( Position, List Position )
    , distance : Maybe Int
    , intersection : Maybe ( Position, Int )
    , totalMoves : Int
    }


type Msg
    = NextMove Direction
    | MoveForward Direction Int


type Direction
    = North
    | East
    | South
    | West


type alias Position =
    ( Int, Int )


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
    { moves = parsedInput
    , positions = ( ( 0, 0 ), [] )
    , distance = Nothing
    , intersection = Nothing
    , totalMoves = List.length parsedInput
    }
        ! [ trigger False <| NextMove North ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextMove dir ->
            case model.moves of
                [] ->
                    { model | distance = Just <| distance <| Tuple.first model.positions } ! []

                move :: rest ->
                    let
                        nextDirection =
                            turn move dir

                        steps =
                            blocks move
                    in
                        { model | moves = rest } ! [ trigger False <| MoveForward nextDirection steps ]

        MoveForward dir steps ->
            moveForward model dir steps


trigger : Bool -> msg -> Cmd msg
trigger delay msg =
    if delay == True then
        Process.sleep (5 * millisecond) |> Task.perform (\_ -> msg)
    else
        Task.perform identity <| Task.succeed msg


moveForward : Model -> Direction -> Int -> ( Model, Cmd Msg )
moveForward model direction steps =
    let
        ( ( x, y ) as position, visited ) =
            model.positions
    in
        if steps == 0 then
            { model
                | positions = ( position, position :: visited )
            }
                ! [ trigger True <| NextMove direction ]
        else
            let
                nextPosition =
                    case direction of
                        North ->
                            ( x, y + 1 )

                        East ->
                            ( x + 1, y )

                        South ->
                            ( x, y - 1 )

                        West ->
                            ( x - 1, y )

                intersection =
                    case model.intersection of
                        Just _ ->
                            model.intersection

                        Nothing ->
                            if List.member nextPosition visited then
                                Just ( nextPosition, distance nextPosition )
                            else
                                Nothing
            in
                { model
                    | positions = ( nextPosition, nextPosition :: visited )
                    , intersection = intersection
                }
                    ! [ trigger True <| MoveForward direction <| steps - 1 ]


view : Model -> Html msg
view model =
    div []
        [ div [] [ Html.text "Part 1: ", formatDistance model.distance model.moves model.totalMoves ]
        , div [] [ Html.text ("Part 2: " ++ (formatIntersection model.intersection)) ]
        , drawGrid model
        ]


formatDistance : Maybe Int -> List Move -> Int -> Html msg
formatDistance distance moves total =
    case distance of
        Just dist ->
            Html.text <| toString dist

        Nothing ->
            case List.length moves of
                0 ->
                    Html.text ""

                len ->
                    let
                        progressWidth =
                            toString <| (200 // total) * len
                    in
                        svg [ width "200", height "10" ] [ rect [ x "0", y "0", height "10", width progressWidth ] [] ]


formatIntersection : Maybe ( Position, Int ) -> String
formatIntersection intersection =
    case intersection of
        Just ( _, distance ) ->
            toString distance

        Nothing ->
            "Not found"


squareSize : Int
squareSize =
    3


toSquare : Position -> Maybe ( Position, Int ) -> Int -> Int -> Position -> Svg msg
toSquare current intersection minX minY (( a, b ) as position) =
    rect
        [ x <| toString <| (a + abs minX) * squareSize
        , y <| toString <| (b + abs minY) * squareSize
        , width <| toString squareSize
        , height <| toString squareSize
        , fill <|
            if position == current then
                "red"
            else
                case intersection of
                    Just ( int, _ ) ->
                        if int == position then
                            "yellow"
                        else
                            "black"

                    Nothing ->
                        "black"
        ]
        []


drawGrid : Model -> Html msg
drawGrid model =
    let
        ( current, visited ) =
            model.positions

        ( minX, maxX ) =
            getMinMax <| List.map Tuple.first visited

        ( minY, maxY ) =
            getMinMax <| List.map Tuple.second visited

        gridWidth =
            toString <| (maxX - minX) * squareSize

        gridHeight =
            toString <| (maxY - minY) * squareSize

        mapFunc =
            toSquare current model.intersection minX minY
    in
        svg [ width gridWidth, height gridHeight ] <| List.map mapFunc visited


getMinMax : List number -> ( number, number )
getMinMax numberList =
    ( getMin numberList - 1, getMax numberList + 1 )


getMin : List number -> number
getMin =
    Maybe.withDefault 0 << List.minimum


getMax : List number -> number
getMax =
    Maybe.withDefault 0 << List.maximum


distance : Position -> Int
distance ( x, y ) =
    (abs x) + (abs y)


turn : Move -> Direction -> Direction
turn move direction =
    case ( move, direction ) of
        ( L _, North ) ->
            West

        ( L _, East ) ->
            North

        ( L _, South ) ->
            East

        ( L _, West ) ->
            South

        ( R _, South ) ->
            West

        ( R _, West ) ->
            North

        ( R _, North ) ->
            East

        ( R _, East ) ->
            South


blocks : Move -> Int
blocks move =
    case move of
        L x ->
            x

        R x ->
            x
