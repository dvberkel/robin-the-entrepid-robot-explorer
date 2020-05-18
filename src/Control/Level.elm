module Control.Level exposing (Level, Msg(..), decode, encode, levelIndex, level, load, name, update, view)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
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


levelIndex : Level -> Int
levelIndex (Level { index }) =
    index


encode : Level -> Encode.Value
encode (Level aLevel) =
    Encode.object
        [ ( "index", Encode.int aLevel.index )
        , ( "world", World.encode aLevel.world )
        ]


decode : Decoder Level
decode =
    Decode.succeed level
        |> Pipeline.required "index" Decode.int
        |> Pipeline.required "world" World.decode


type Msg
    = Step


update : Msg -> Level -> ( Level, Cmd Msg )
update message aLevel =
    case message of
        Step ->
            ( step aLevel, Cmd.none )


view : Level -> Html Msg
view (Level aLevel) =
    Html.div [ Attribute.css [ displayFlex, flexDirection column, flexWrap noWrap, justifyContent flexStart, alignItems flexStart ] ]
        [ Html.button [ Event.onClick Step ] [ Html.text "⏵" ]
        , World.view aLevel.world
        , Html.div [] <| List.map Html.text <| List.map Robot.instructionToString aLevel.instructions
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


step : Level -> Level
step (Level aLevel) =
    let
        nextWorld =
            case World.executeAll aLevel.instructions aLevel.world of
                Ok w ->
                    w

                -- TODO error handling
                Err _ ->
                    aLevel.world
    in
    Level { aLevel | world = nextWorld }
