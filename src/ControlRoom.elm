module ControlRoom exposing (..)

import Browser
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


type alias Model =
    {}


view : Model -> Browser.Document Msg
view _ =
    { title = "Control Room"
    , body = [ Html.text "Hello, World!" ]
    }


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
