module World.GPS.Direction exposing (Direction(..), toLeft, toRight)


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
