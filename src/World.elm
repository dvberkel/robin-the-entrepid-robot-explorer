module World exposing (Error, Instruction(..), World, execute)


type World
    = World
        { robot : Robot
        }


execute : List Instruction -> World -> Result Error World
execute instructions start =
    let
        actOn : Instruction -> Result Error World -> Result Error World
        actOn instruction world =
            Result.andThen (perform instruction) world
    in
    List.foldl actOn (Ok start) instructions


perform : Instruction -> World -> Result Error World
perform instruction (World world) =
    Ok <| World { world | robot = interpret instruction world.robot }


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
