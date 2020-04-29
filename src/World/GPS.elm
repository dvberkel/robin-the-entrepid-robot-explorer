module World.GPS exposing (Direction(..), Location, advance, coordinates2D, location, toLeft, toRight)


type Location
    = Location { x : Int, y : Int }


location : Int -> Int -> Location
location x y =
    Location { x = x, y = y }


coordinates2D : Location -> ( Int, Int )
coordinates2D (Location { x, y }) =
    ( x, y )


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
