module World exposing (Error, World, executeAll, world)

import World.Maze as Maze exposing (Maze, elementAt)
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
            (index, instruction)

        actOn : (Int, Instruction) -> Result Error World -> Result Error World
        actOn instruction aWorld =
            Result.andThen (execute instruction) aWorld
    in
    instructions
    |> List.indexedMap enumerate
    |> List.foldl actOn (Ok start)


execute : (Int, Instruction) -> World -> Result Error World
execute (_, instruction) (World aWorld) =
    Ok <| World { aWorld | robot = Robot.execute instruction aWorld.robot }


type Error
    = HitAWall
    | FellInAPit
