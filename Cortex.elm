module Cortex exposing (..)

import Html exposing (..)
import Char exposing (fromCode)
import Keyboard


-- MODEL


type alias Model =
    { buffer : List String
    , cursorPosition : CursorPosition
    }


type alias CursorPosition =
    { x : Int
    , y : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { buffer = [ "" ], cursorPosition = { x = 0, y = 0 } }, Cmd.none )



-- UPDATE


type Msg
    = Presses Char.KeyCode


insertAtLine : Int -> Int -> String -> List String -> List String
insertAtLine targetLine i newString list =
    if targetLine == i then
        case list of
            [] ->
                []

            [ x ] ->
                [ (String.append x newString) ]

            x :: xs ->
                (String.append x newString) :: (insertAtLine targetLine (i + 1) newString xs)
    else
        case list of
            [] ->
                []

            [ x ] ->
                [ x ]

            x :: xs ->
                x :: (insertAtLine targetLine (i + 1) newString xs)


insertNewline : Model -> Model
insertNewline model =
    let
        newBuffer =
            List.concat [ model.buffer, [ "" ] ]
    in
        { model | buffer = newBuffer, cursorPosition = { x = 0, y = model.cursorPosition.y + 1 } }


insertChar : Char -> Model -> Model
insertChar char model =
    let
        newBuffer =
            insertAtLine model.cursorPosition.y 0 (String.fromChar char) model.buffer
    in
        { model | buffer = newBuffer, cursorPosition = { x = model.cursorPosition.x + 1, y = model.cursorPosition.y } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Presses code ->
            case code of
                -- Enter
                13 ->
                    ( insertNewline model, Cmd.none )

                -- Other Chars
                _ ->
                    ( insertChar (fromCode code) model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> Presses code)



-- VIEW


displayBuffer : List String -> List (Html msg)
displayBuffer buffer =
    let
        displayBufferLine line =
            div []
                [ text line ]
    in
        List.map displayBufferLine buffer


view : Model -> Html Msg
view model =
    div []
        [ main_ [] (displayBuffer model.buffer)
        , aside [] [ text (toString model) ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
