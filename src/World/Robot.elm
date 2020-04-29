module World.Robot exposing (Instruction(..), Robot, execute, robot)

import World.GPS exposing (Direction, Location, advance, toLeft, toRight)


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
