module Json.Control.Level exposing (decode, encode)

import Control.Level exposing (Level, level, levelIndex)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.World as JsonWorld


encode : Level -> Encode.Value
encode aLevel =
    Encode.object
        [ ( "index", Encode.int <| levelIndex aLevel )
        , ( "world", JsonWorld.encode <| world aLevel )
        ]


decode : Decoder Level
decode =
    Decode.succeed level
        |> Pipeline.required "index" Decode.int
        |> Pipeline.required "world" JsonWorld.decode
