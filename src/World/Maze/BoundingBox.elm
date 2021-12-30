module World.Maze.BoundingBox exposing (BoundingBox, allLocations, bbox, default, lowerLeft, upperRight)

import World.GPS.Location as Location exposing (Location)


type BoundingBox
    = BoundingBox ( Location, Location )


default : BoundingBox
default =
    let
        ll =
            Location.location 0 0

        ur =
            Location.location 1 1
    in
    bbox ll ur


bbox : Location -> Location -> BoundingBox
bbox ll ur =
    BoundingBox ( ll, ur )


lowerLeft : BoundingBox -> Location
lowerLeft (BoundingBox ( ll, _ )) =
    ll


upperRight : BoundingBox -> Location
upperRight (BoundingBox ( _, ur )) =
    ur


allLocations : BoundingBox -> List Location
allLocations (BoundingBox ( ll, ur )) =
    let
        ( llx, lly ) =
            Location.coordinates2D ll

        ( urx, ury ) =
            Location.coordinates2D ur

        xs =
            List.range llx urx

        ys =
            List.range lly ury
    in
    cartesianProduct xs ys
        |> List.map (uncurry Location.location)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
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
