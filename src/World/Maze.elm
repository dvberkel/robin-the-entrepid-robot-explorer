module World.Maze exposing (Element(..), Maze, elementAt, emptyMaze, insertElement, insertRectangle)

import Dict exposing (Dict)
import World.GPS as GPS exposing (Location)


type Maze
    = Maze (Dict Int (Dict Int Element))


type Element
    = Floor
    | Pit
    | Wall


emptyMaze : Maze
emptyMaze =
    Maze Dict.empty


insertRectangle : ( Location, Location ) -> Element -> Maze -> Maze
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
        |> List.foldl (swap insertElement <| element) aMaze


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


insertElement : Location -> Element -> Maze -> Maze
insertElement location element (Maze locations) =
    let
        ( x, y ) =
            GPS.coordinates2D location

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
elementAt location (Maze dictionary) =
    let
        ( x, y ) =
            GPS.coordinates2D location
    in
    dictionary
        |> Dict.get x
        |> Maybe.withDefault Dict.empty
        |> Dict.get y
        |> Maybe.withDefault Pit
