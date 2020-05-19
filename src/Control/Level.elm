module Control.Level exposing (Level, Msg(..), decode, encode, level, levelIndex, load, name, update, view)

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
        , instructionPointer : Maybe Int
        }


level : Int -> World -> Level
level index aWorld =
    Level
        { index = index
        , world = aWorld
        , instructions = []
        , instructionPointer = Nothing
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
    = Reset
    | Step


update : Msg -> Level -> ( Level, Cmd Msg )
update message aLevel =
    case message of
        Reset ->
            ( reset aLevel, Cmd.none )

        Step ->
            ( step aLevel, Cmd.none )


view : Level -> Html Msg
view (Level aLevel) =
    let
        instructions =
            aLevel.instructions
                |> List.map Robot.instructionToString
                |> String.join ","

        text =
            "[" ++ instructions ++ "]"
    in
    Html.div [ Attribute.css [ displayFlex, flexDirection column, flexWrap noWrap, justifyContent flexStart, alignItems flexStart ] ]
        [ Html.div []
            [ Html.button [ Event.onClick Reset ] [ Html.text "↻" ]
            , Html.button [ Event.onClick Step ] [ Html.text "⏵" ]
            ]
        , World.view aLevel.world
        , Html.div [] [ Html.text text ]
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
    Level
        { aLevel
            | instructions = instructions
            , instructionPointer =
                instructions
                    |> List.head
                    |> Maybe.map (\_ -> 0)
        }


reset : Level -> Level
reset (Level aLevel) =
    Level
        { aLevel
            | world = World.reset aLevel.world
            , instructionPointer = Maybe.map (\_ -> 0) aLevel.instructionPointer
        }


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
