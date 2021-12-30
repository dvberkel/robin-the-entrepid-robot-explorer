module World.GPS.Location exposing (Location, ParseError(..), coordinates2D, location, parse, toString)


type Location
    = Location { x : Int, y : Int }


location : Int -> Int -> Location
location x y =
    Location { x = x, y = y }


coordinates2D : Location -> ( Int, Int )
coordinates2D (Location { x, y }) =
    ( x, y )


toString : Location -> String
toString (Location { x, y }) =
    let
        sx =
            String.fromInt x

        sy =
            String.fromInt y
    in
    sx ++ "," ++ sy


parse : String -> Result ParseError Location
parse input =
    let
        coordinates =
            input
                |> String.split ","
                |> List.map String.toInt
    in
    case coordinates of
        [ Just x, Just y ] ->
            Ok <| location x y

        _ ->
            Err <| NotALocation input


type ParseError
    = NotALocation String
