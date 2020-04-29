module World.Maze exposing (Tile(..), Maze, tileAt, emptyMaze, insertTile, insertRectangle)

import Dict exposing (Dict)
import World.GPS as GPS exposing (Location)


type Maze
    = Maze (Dict Int (Dict Int Tile))


type Tile
    = Floor
    | Pit
    | Wall


emptyMaze : Maze
emptyMaze =
    Maze Dict.empty


insertRectangle : ( Location, Location ) -> Tile -> Maze -> Maze
insertRectangle ( lowerLeft, upperRight ) element aMaze =
    let
        ( llx, lly ) =
            GPS.coordinates2D lowerLeft

        ( urx, ury ) =
            GPS.coordinates2D upperRight

        xs =
            List.range llx urx

        ys =
            List.range lly ury
    in
    cartesianProduct xs ys
        |> List.map (unpack GPS.location)
        |> List.foldl (swap insertTile <| element) aMaze


unpack : (a -> b -> c) -> ( a, b ) -> c
unpack f ( a, b ) =
    f a b


swap : (a -> b -> c) -> b -> a -> c
swap f b a =
    f a b


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    let
        columnOf x =
            ys
                |> List.map (\y -> ( x, y ))
    in
    xs
        |> List.concatMap columnOf


insertTile : Location -> Tile -> Maze -> Maze
insertTile location tile (Maze locations) =
    let
        ( x, y ) =
            GPS.coordinates2D location

        insert : Maybe (Dict Int Tile) -> Maybe (Dict Int Tile)
        insert dictionary =
            dictionary
                |> Maybe.withDefault Dict.empty
                |> Dict.insert y tile
                |> Just
    in
    locations
        |> Dict.update x insert
        |> Maze


tileAt : Location -> Maze -> Tile
tileAt location (Maze dictionary) =
    let
        ( x, y ) =
            GPS.coordinates2D location
    in
    dictionary
        |> Dict.get x
        |> Maybe.withDefault Dict.empty
        |> Dict.get y
        |> Maybe.withDefault Pit
