module Control.ControlRoom exposing (ControlRoom, Level, controlRoom, decode, encode, level, levelName, view)

import Html exposing (Html)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World exposing (World)


type alias ControlRoom =
    { level : Level
    , code : String
    }


controlRoom : Level -> ControlRoom
controlRoom aLevel =
    { level = aLevel
    , code = ""
    }


type alias Level =
    { index : Int
    , world : World
    }


level : Int -> World -> Level
level index aWorld =
    { index = index
    , world = aWorld
    }


encode : Level -> Encode.Value
encode aLevel =
    Encode.object
        [ ( "index", Encode.int aLevel.index )
        , ( "world", World.encode aLevel.world )
        ]


decode : Decoder Level
decode =
    Decode.succeed Level
        |> required "index" Decode.int
        |> required "world" World.decode


view : ControlRoom -> List (Html msg)
view aControlRoom =
    [ Html.h1 [] [ Html.text <| "Level " ++ levelName aControlRoom.level.index ]
    , World.view aControlRoom.level.world
    ]


levelName : Int -> String
levelName index =
    let
        prefix =
            if index < 10 then
                "0"

            else
                ""
    in
    prefix ++ String.fromInt index
