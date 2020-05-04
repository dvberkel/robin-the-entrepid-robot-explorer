module Control.ControlRoom exposing (ControlRoom, Msg, controlRoom, update, view)

import Control.Level as Level exposing (Level)
import Editor exposing (Editor)
import EditorMsg exposing (EMsg)
import Html exposing (Html)


type ControlRoom
    = ControlRoom
        { level : Level
        , editor : Editor
        }


controlRoom : Level -> ControlRoom
controlRoom aLevel =
    ControlRoom
        { level = aLevel
        , editor = Editor.initWithContent "Forward" ()
        }


type Msg
    = EditorMsg EditorMsg.EMsg


update : Msg -> ControlRoom -> ( ControlRoom, Cmd Msg )
update message (ControlRoom aControlRoom) =
    case message of
        EditorMsg editorMessage ->
            let
                ( editor, cmd ) =
                    Editor.update editorMessage aControlRoom.editor
            in
            ( ControlRoom { aControlRoom | editor = editor }, Cmd.map EditorMsg cmd )


view : ControlRoom -> List (Html Msg)
view (ControlRoom aControlRoom) =
    [ Html.h1 [] [ Html.text "Control" ]
    , Level.view aControlRoom.level
    , Editor.view aControlRoom.editor |> Html.map EditorMsg
    ]
