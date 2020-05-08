module World.Robot exposing (Instruction(..), Robot, decode, encode, execute, location, robot, view)

import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attribute
import World.GPS as GPS exposing (Direction(..), Location, advance, toLeft, toRight)


type Robot
    = Robot
        { location : Location
        , direction : Direction
        }


robot : Direction -> Location -> Robot
robot direction aLocation =
    Robot { direction = direction, location = aLocation }


execute : Instruction -> Robot -> Robot
execute instruction aRobot =
    case instruction of
        Forward ->
            forward aRobot

        Left ->
            left aRobot

        Right ->
            right aRobot


type Instruction
    = Forward
    | Left
    | Right


forward : Robot -> Robot
forward (Robot aRobot) =
    Robot { aRobot | location = advance aRobot.direction aRobot.location }


left : Robot -> Robot
left (Robot aRobot) =
    Robot { aRobot | direction = toLeft aRobot.direction }


right : Robot -> Robot
right (Robot aRobot) =
    Robot { aRobot | direction = toRight aRobot.direction }


location : Robot -> Location
location (Robot aRobot) =
    aRobot.location


encode : Robot -> Encode.Value
encode (Robot aRobot) =
    Encode.object
        [ ( "location", GPS.encodeLocation aRobot.location )
        , ( "direction", GPS.encodeDirection aRobot.direction )
        ]


decode : Decoder Robot
decode =
    Decode.succeed robot
        |> required "direction" GPS.decodeDirection
        |> required "location" GPS.decodeLocation


view : Robot -> Html msg
view (Robot aRobot) =
    let
        ( x, y ) =
            GPS.coordinates2D aRobot.location

        toTranslate coordinates =
            "translate(" ++ coordinates ++ ")"

        translate =
            [ x, y ]
                |> List.map toFloat
                |> List.map (\z -> z + 0.5)
                |> List.map String.fromFloat
                |> String.join ","
                |> toTranslate

        orientation =
            case aRobot.direction of
                North ->
                    "rotate(0)"

                East ->
                    "rotate(90)"

                South ->
                    "rotate(180)"

                West ->
                    "rotate(-90)"
    in
    Svg.g [ Attribute.transform translate ]
        [ Svg.g [ Attribute.transform <| String.join " " [ orientation, "scale(0.7978)" ] ]
            [ Svg.circle
                [ Attribute.cx "0"
                , Attribute.cy "0"
                , Attribute.r "0.5"
                , Attribute.fill "silver"
                ]
                []
            , Svg.circle
                [ Attribute.cx "0"
                , Attribute.cy "0.4"
                , Attribute.r "0.1"
                , Attribute.fill "green"
                ]
                []
            ]
        ]
