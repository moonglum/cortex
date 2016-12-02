module Cortex exposing (..)

import Html exposing (..)
import Char exposing (fromCode)
import Keyboard


-- MODEL

type alias Model =
    { buffer: String
    }


init : (Model, Cmd Msg)
init =
    ({ buffer = "" }, Cmd.none)


-- UPDATE

type Msg = Presses Char.KeyCode


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Presses code ->
            case code of
                13 -> ({ model | buffer = ""}, Cmd.none)
                _ -> ({ model | buffer = String.append model.buffer (String.fromChar (fromCode code)) }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> Presses code)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ text model.buffer ]


-- MAIN

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
