module Day01.Day1 exposing (main)

import Browser
import Day01.Input exposing (Move(..), parsedInput)
import Html exposing (Html, div, text)
import Process exposing (sleep)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)
import Task


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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { moves = parsedInput
      , positions = ( ( 0, 0 ), [] )
      , distance = Nothing
      , intersection = Nothing
      , totalMoves = List.length parsedInput
      }
    , trigger False <| NextMove North
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextMove dir ->
            case model.moves of
                [] ->
                    ( { model
                        | distance = Just <| manhattan <| Tuple.first model.positions
                      }
                    , Cmd.none
                    )

                move :: rest ->
                    let
                        nextDirection =
                            turn move dir

                        steps =
                            blocks move
                    in
                    ( { model
                        | moves = rest
                      }
                    , trigger False <| MoveForward nextDirection steps
                    )

        MoveForward dir steps ->
            moveForward model dir steps


trigger : Bool -> msg -> Cmd msg
trigger delay msg =
    if delay == True then
        Process.sleep 5 |> Task.perform (\_ -> msg)

    else
        Task.perform identity <| Task.succeed msg


moveForward : Model -> Direction -> Int -> ( Model, Cmd Msg )
moveForward model direction steps =
    let
        ( ( x, y ) as position, visited ) =
            model.positions
    in
    if steps == 0 then
        ( { model
            | positions = ( position, position :: visited )
          }
        , trigger True <| NextMove direction
        )

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
                            Just ( nextPosition, manhattan nextPosition )

                        else
                            Nothing
        in
        ( { model
            | positions = ( nextPosition, nextPosition :: visited )
            , intersection = intersection
          }
        , trigger True <| MoveForward direction <| steps - 1
        )


view : Model -> Html msg
view model =
    div []
        [ div [] [ text "Part 1: ", formatDistance model.distance model.moves model.totalMoves ]
        , div [] [ text ("Part 2: " ++ formatIntersection model.intersection) ]
        , drawGrid model
        ]


formatDistance : Maybe Int -> List Move -> Int -> Html msg
formatDistance distance moves total =
    case distance of
        Just dist ->
            text <| String.fromInt dist

        Nothing ->
            case List.length moves of
                0 ->
                    text ""

                len ->
                    svg
                        [ width "200"
                        , height "10"
                        ]
                        [ rect
                            [ x "0"
                            , y "0"
                            , height "10"
                            , width <| String.fromInt <| (200 // total) * len
                            ]
                            []
                        ]


formatIntersection : Maybe ( Position, Int ) -> String
formatIntersection intersection =
    case intersection of
        Just ( _, distance ) ->
            String.fromInt distance

        Nothing ->
            "Not found"


squareSize : Int
squareSize =
    3


toSquare : Position -> Maybe ( Position, Int ) -> Int -> Int -> Position -> Svg msg
toSquare current intersection minX minY (( a, b ) as position) =
    rect
        [ x <| String.fromInt <| (a + abs minX) * squareSize
        , y <| String.fromInt <| (b + abs minY) * squareSize
        , width <| String.fromInt squareSize
        , height <| String.fromInt squareSize
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
            String.fromInt <| (maxX - minX) * squareSize

        gridHeight =
            String.fromInt <| (maxY - minY) * squareSize

        mapFunc =
            toSquare current model.intersection minX minY
    in
    svg
        [ width gridWidth
        , height gridHeight
        ]
    <|
        List.map mapFunc visited


getMinMax : List number -> ( number, number )
getMinMax numberList =
    ( getMin numberList - 1, getMax numberList + 1 )


getMin : List number -> number
getMin =
    List.minimum >> Maybe.withDefault 0


getMax : List number -> number
getMax =
    List.maximum >> Maybe.withDefault 0


manhattan : Position -> Int
manhattan ( x, y ) =
    abs x + abs y


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
