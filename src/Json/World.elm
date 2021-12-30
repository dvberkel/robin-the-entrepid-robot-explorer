module Json.World exposing (decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World exposing (World)
import Json.World.Maze as Maze
import Json.World.Robot as Robot


encode : World -> Encode.Value
encode world =
    let
        robot =
            World.robot world

        maze =
            World.maze world
    in
    Encode.object
        [ ( "robot", Robot.encode robot )
        , ( "maze", Maze.encode maze )
        ]


decode : Decoder World
decode =
    Decode.succeed World.world
        |> required "maze" Maze.decode
        |> required "robot" Robot.decode
