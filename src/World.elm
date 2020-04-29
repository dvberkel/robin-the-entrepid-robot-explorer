module World exposing (Error(..), World, executeAll, world)

import World.GPS exposing (Location)
import World.Maze as Maze exposing (Tile(..), Maze, tileAt)
import World.Robot as Robot exposing (Instruction, Robot)


type World
    = World
        { robot : Robot
        , maze : Maze
        }


world : Maze -> Robot -> World
world aMaze aRobot =
    World
        { robot = aRobot
        , maze = aMaze
        }


executeAll : List Instruction -> World -> Result Error World
executeAll instructions start =
    let
        enumerate index instruction =
            ( index, instruction )

        actOn : ( Int, Instruction ) -> Result Error World -> Result Error World
        actOn instruction aWorld =
            Result.andThen (execute instruction) aWorld
    in
    instructions
        |> List.indexedMap enumerate
        |> List.foldl actOn (Ok start)


execute : ( Int, Instruction ) -> World -> Result Error World
execute ( instructionPointer, instruction ) (World aWorld) =
    let
        intention =
            Robot.execute instruction aWorld.robot

        targetLocation =
            Robot.location intention

        targetTile =
            tileAt targetLocation aWorld.maze
    in
    case targetTile of
        Floor ->
            Ok <| World { aWorld | robot = intention }

        Wall ->
            Err <| HitAWall instructionPointer targetLocation

        Pit ->
            Err <| FellInAPit instructionPointer targetLocation


type Error
    = HitAWall Int Location
    | FellInAPit Int Location
