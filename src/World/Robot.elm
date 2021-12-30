module World.Robot exposing (Robot, direction, execute, location, robot, view)

import Html.Styled exposing (Html)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attribute
import World.GPS as GPS
import World.GPS.Direction as Direction exposing (Direction(..))
import World.GPS.Location as Location exposing (Location)
import World.Robot.Instruction exposing (Instruction(..))


type Robot
    = Robot
        { location : Location
        , direction : Direction
        }


robot : Direction -> Location -> Robot
robot aDirection aLocation =
    Robot { direction = aDirection, location = aLocation }


execute : Instruction -> Robot -> Robot
execute instruction aRobot =
    case instruction of
        Forward ->
            forward aRobot

        Left ->
            left aRobot

        Right ->
            right aRobot


forward : Robot -> Robot
forward (Robot aRobot) =
    Robot { aRobot | location = GPS.advance aRobot.direction aRobot.location }


left : Robot -> Robot
left (Robot aRobot) =
    Robot { aRobot | direction = Direction.toLeft aRobot.direction }


right : Robot -> Robot
right (Robot aRobot) =
    Robot { aRobot | direction = Direction.toRight aRobot.direction }


location : Robot -> Location
location (Robot aRobot) =
    aRobot.location


direction : Robot -> Direction
direction (Robot aRobot) =
    aRobot.direction


view : Robot -> Html msg
view (Robot aRobot) =
    let
        ( x, y ) =
            Location.coordinates2D aRobot.location

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
