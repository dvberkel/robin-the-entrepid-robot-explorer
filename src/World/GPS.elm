module World.GPS exposing (advance)

import World.GPS.Direction exposing (Direction(..))
import World.GPS.Location as Location exposing (Location)


advance : Direction -> Location -> Location
advance direction location =
    let
        ( x, y ) =
            Location.coordinates2D location
    in
    case direction of
        North ->
            Location.location x (y + 1)

        East ->
            Location.location (x + 1) y

        South ->
            Location.location x (y - 1)

        West ->
            Location.location (x - 1) y
