module World exposing (Direction(..), Error, Instruction(..), Location, Robot, World, executeAll, location, robot, world)


type World
    = World
        { robot : Robot
        }


world : Robot -> World
world aRobot =
    World { robot = aRobot }


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
interpret instruction aRobot =
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


type Error
    = HitAWall


type Robot
    = Robot
        { location : Location
        , direction : Direction
        }


robot : Direction -> Location -> Robot
robot direction aLocation =
    Robot { direction = direction, location = aLocation }


forward : Robot -> Robot
forward (Robot aRobot) =
    Robot { aRobot | location = advance aRobot.direction aRobot.location }


left : Robot -> Robot
left (Robot aRobot) =
    Robot { aRobot | direction = toLeft aRobot.direction }


right : Robot -> Robot
right (Robot aRobot) =
    Robot { aRobot | direction = toRight aRobot.direction }


type Location
    = Location { x : Int, y : Int }

location : Int -> Int -> Location
location x y =
    Location { x = x, y = y }

advance : Direction -> Location -> Location
advance direction (Location ({ x, y } as aLocation)) =
    case direction of
        North ->
            Location { aLocation | y = y + 1 }

        East ->
            Location { aLocation | x = x + 1 }

        South ->
            Location { aLocation | y = y - 1 }

        West ->
            Location { aLocation | x = x - 1 }


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
