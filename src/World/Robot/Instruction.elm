module World.Robot.Instruction exposing (Instruction(..), toString)


type Instruction
    = Forward
    | Left
    | Right


toString : Instruction -> String
toString instruction =
    case instruction of
        Forward ->
            "Forward"

        Left ->
            "Left"

        Right ->
            "Right"
