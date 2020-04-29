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
    ( Loaded { level = 0 }, Cmd.none )


type Model
    = Loading Int
    | Loaded Level


type alias Level =
    { level : Int
    }


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Loading level ->
                    connectingToLevel level

                Loaded level ->
                    controlLevel level
    in
    { title = "Control Room"
    , body = body
    }


connectingToLevel : Int -> List (Html Msg)
connectingToLevel index =
    [ Html.h1 [] [ Html.text "patching into CCTV stream" ] ]


controlLevel : Level -> List (Html Msg)
controlLevel { level } =
    [ Html.h1 [] [ Html.text <| "Level " ++ String.fromInt level ]
    ]


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
