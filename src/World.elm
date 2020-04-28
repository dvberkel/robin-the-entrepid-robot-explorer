module World exposing (Direction(..), Element, Error, Instruction(..), Maze, Location, Robot, World, emptyMaze, executeAll, insertElement, location, robot, world)

import Dict exposing (Dict)


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


type Maze
    = Maze (Dict Int (Dict Int Element))


emptyMaze : Maze
emptyMaze =
    Maze Dict.empty


insertElement : Location -> Element -> Maze -> Maze
insertElement (Location { x, y }) element (Maze locations) =
    let
        insert : Maybe (Dict Int Element) -> Maybe (Dict Int Element)
        insert dictionary =
            dictionary
                |> Maybe.withDefault Dict.empty
                |> Dict.insert y element
                |> Just
    in
    locations
        |> Dict.update x insert
        |> Maze


elementAt : Location -> Maze -> Element
elementAt (Location { x, y }) (Maze dictionary) =
    dictionary
        |> Dict.get x
        |> Maybe.withDefault Dict.empty
        |> Dict.get y
        |> Maybe.withDefault Pit


type Element
    = Tile
    | Pit
