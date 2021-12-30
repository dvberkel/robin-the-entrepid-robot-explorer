module Control.Level exposing (Level, Msg(..), decode, encode, level, levelIndex, load, name, update, view)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.World as JsonWorld
import World exposing (Error(..), World)
import World.Robot.Instruction as Instruction exposing (Instruction)


type Level
    = Level
        { index : Int
        , world : World
        , instructions : List Instruction
        , instructionPointer : Maybe Int
        , notification : Notification
        }


type Notification
    = Silent
    | Problem String
    | Announcement String


level : Int -> World -> Level
level index aWorld =
    Level
        { index = index
        , world = aWorld
        , instructions = []
        , instructionPointer = Nothing
        , notification = Silent
        }


levelIndex : Level -> Int
levelIndex (Level { index }) =
    index


encode : Level -> Encode.Value
encode (Level aLevel) =
    Encode.object
        [ ( "index", Encode.int aLevel.index )
        , ( "world", JsonWorld.encode aLevel.world )
        ]


decode : Decoder Level
decode =
    Decode.succeed level
        |> Pipeline.required "index" Decode.int
        |> Pipeline.required "world" JsonWorld.decode


type Msg
    = Reset
    | Step
    | Run


update : Msg -> Level -> ( Level, Cmd Msg )
update message aLevel =
    case message of
        Reset ->
            ( reset aLevel, Cmd.none )

        Step ->
            ( step aLevel, Cmd.none )

        Run ->
            ( run aLevel, Cmd.none )


view : Level -> Html Msg
view (Level aLevel) =
    let
        instructions =
            aLevel.instructions
                |> List.map Instruction.toString
                |> String.join ","

        announcement =
            case aLevel.notification of
                Silent ->
                    ""

                Problem problem ->
                    problem

                Announcement message ->
                    message
    in
    Html.div [ Attribute.css [ displayFlex, flexDirection column, flexWrap noWrap, justifyContent flexStart, alignItems flexStart ] ]
        [ Html.div []
            [ Html.button [ Event.onClick Reset ] [ Html.text "↻" ]
            , Html.button [ Event.onClick Step ] [ Html.text "→" ]
            , Html.button [ Event.onClick Run ] [ Html.text "⇥" ]
            ]
        , World.view aLevel.world
        , Html.div [ Attribute.css [ backgroundColor (rgb 211 211 211) ] ] [ Html.text instructions ]
        , Html.div [] [ Html.text announcement ]
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


load : List Instruction -> Level -> Level
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
        instruction =
            aLevel.instructionPointer
                |> Maybe.map (\index -> List.take 1 <| List.drop index aLevel.instructions)
                |> Maybe.withDefault []

        result =
            World.executeAll instruction aLevel.world

        nextWorld =
            result
                |> Result.withDefault (World.reset aLevel.world)

        notification =
            case result of
                Ok _ ->
                    Silent

                Err (HitAWall instructionPointer _) ->
                    Problem <| "hit a wall trying to execute instruction #" ++ String.fromInt instructionPointer

                Err (FellInAPit instructionPointer _) ->
                    Problem <| "fell in a pit trying to execute instruction #" ++ String.fromInt instructionPointer

        aInstructionPointer =
            case ( aLevel.instructionPointer, result ) of
                ( Just n, Ok _ ) ->
                    Just <| n + 1

                _ ->
                    aLevel.instructionPointer
    in
    Level { aLevel | world = nextWorld, notification = notification, instructionPointer = aInstructionPointer }


run : Level -> Level
run (Level aLevel) =
    let
        result =
            World.executeAll aLevel.instructions aLevel.world

        nextWorld =
            result
                |> Result.withDefault (World.reset aLevel.world)

        notification =
            case result of
                Ok _ ->
                    Silent

                Err (HitAWall instructionPointer _) ->
                    Problem <| "hit a wall trying to execute instruction #" ++ String.fromInt instructionPointer

                Err (FellInAPit instructionPointer _) ->
                    Problem <| "fell in a pit trying to execute instruction #" ++ String.fromInt instructionPointer

        aInstructionPointer =
            case result of
                Ok _ ->
                    Just <| List.length aLevel.instructions

                Err (HitAWall instructionPointer _) ->
                    Just instructionPointer

                Err (FellInAPit instructionPointer _) ->
                    Just instructionPointer
    in
    Level { aLevel | world = nextWorld, notification = notification, instructionPointer = aInstructionPointer }
