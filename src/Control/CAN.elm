module Control.CAN exposing (Error(..), parse)

import World.Robot.Instruction exposing (Instruction(..))


parse : String -> Result Error (List Instruction)
parse _ =
    Ok [ Forward, Forward, Forward, Forward ]


type Error
    = GenericParseProblem
