module Control.Level exposing (Level, decode, encode, level, name, load, view)

import Html.Styled as Html exposing (Html)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World exposing (World)
import World.Robot as Robot


type Level
    = Level
        { index : Int
        , world : World
        , instructions : List Robot.Instruction
        }


level : Int -> World -> Level
level index aWorld =
    Level
        { index = index
        , world = aWorld
        , instructions = []
        }


encode : Level -> Encode.Value
encode (Level aLevel) =
    Encode.object
        [ ( "index", Encode.int aLevel.index )
        , ( "world", World.encode aLevel.world )
        ]


decode : Decoder Level
decode =
    Decode.succeed level
        |> required "index" Decode.int
        |> required "world" World.decode


view : Level -> Html msg
view (Level aLevel) =
    Html.div []
        [ World.view aLevel.world
        ]


name : Int -> String
name index =
    let
        prefix =
            if index < 10 then
                "0"

            else
                ""
    in
    prefix ++ String.fromInt index


load : List Robot.Instruction -> Level -> Level
load instructions (Level aLevel) =
    Level { aLevel | instructions = instructions }
