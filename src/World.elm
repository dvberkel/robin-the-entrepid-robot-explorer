module World exposing (Error, Instruction(..), World, executeAll, world)


type World
    = World
        { robot : Robot
        }


world : Robot -> World
world robot =
    World { robot = robot }


executeAll : List Instruction -> World -> Result Error World
executeAll instructions start =
    let
        actOn : Instruction -> Result Error World -> Result Error World
        actOn instruction aWorld =
            Result.andThen (execute instruction) aWorld
    in
    List.foldl actOn (Ok start) instructions


execute : Instruction -> World -> Result Error World
execute instruction (World aWorld) =
    Ok <| World { aWorld | robot = interpret instruction aWorld.robot }


interpret : Instruction -> Robot -> Robot
interpret instruction robot =
    case instruction of
        Forward ->
            forward robot

        Left ->
            left robot

        Right ->
            right robot


type Instruction
    = Forward
    | Left
    | Right


type Error
    = HitAWall


type Robot
    = Robot
        { location : Position
        , direction : Direction
        }


forward : Robot -> Robot
forward (Robot robot) =
    Robot { robot | location = advance robot.direction robot.location }


left : Robot -> Robot
left (Robot robot) =
    Robot { robot | direction = toLeft robot.direction }


right : Robot -> Robot
right (Robot robot) =
    Robot { robot | direction = toRight robot.direction }


type Position
    = Position { x : Int, y : Int }


advance : Direction -> Position -> Position
advance direction (Position { x, y }) =
    case direction of
        North ->
            Position { x = x, y = y + 1 }

        East ->
            Position { x = x + 1, y = y }

        South ->
            Position { x = x, y = y + 1 }

        West ->
            Position { x = x - 1, y = y }


type Direction
    = North
    | East
    | South
    | West


toLeft : Direction -> Direction
toLeft direction =
    case direction of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


toRight : Direction -> Direction
toRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North
