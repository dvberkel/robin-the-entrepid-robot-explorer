module Control.ControlRoom exposing (ControlRoom, controlRoom, view)

import Control.Level as Level exposing (Level)
import Html exposing (Html)


type alias ControlRoom =
    { level : Level
    , code : String
    }


controlRoom : Level -> ControlRoom
controlRoom aLevel =
    { level = aLevel
    , code = ""
    }


view : ControlRoom -> List (Html msg)
view aControlRoom =
    [ Html.h1 [] [ Html.text "Control" ]
    , Level.view aControlRoom.level
    ]
