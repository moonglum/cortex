module Cortex exposing (..)

import Html exposing (..)
import Char exposing (fromCode)
import Keyboard


-- MODEL

type alias Model =
    { buffer: String -- TODO: Array of Strings
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
                -- Enter
                13 ->
                    let
                        newBuffer = String.append model.buffer "\n"
                    in
                        ({ model | buffer = newBuffer }, Cmd.none)

                -- Other Chars
                _ ->
                    let
                        newBuffer =
                            code
                            |> fromCode
                            |> String.fromChar
                            |> String.append model.buffer
                    in
                        ({ model | buffer = newBuffer }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> Presses code)


-- VIEW

view : Model -> Html Msg
view model =
    main_ []
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
