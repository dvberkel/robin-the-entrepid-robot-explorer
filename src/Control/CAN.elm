module Control.CAN exposing (Error(..), parse)

import World.Robot exposing (Instruction(..))


parse : String -> Result Error (List Instruction)
parse _ =
    Ok [ Forward ]


type Error
    = GenericParseProblem
