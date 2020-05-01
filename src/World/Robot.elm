module World.Robot exposing (Instruction(..), Robot, decode, encode, execute, location, robot)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World.GPS as GPS exposing (Direction, Location, advance, toLeft, toRight)


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
