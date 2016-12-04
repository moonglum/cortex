module Cortex exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
insertAtLine targetLine y newString list =
    let
        helper line =
            if targetLine == y then
                String.append line newString
            else
                line
    in
        case list of
            [] ->
                []

            line :: xs ->
                (helper line) :: (insertAtLine targetLine (y + 1) newString xs)


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


displayBufferChar : CursorPosition -> CursorPosition -> Char -> Html msg
displayBufferChar cursorPosition currentPosition char =
    let
        active =
            (currentPosition.x == cursorPosition.x) && (currentPosition.y == cursorPosition.y)
    in
        span [ classList [ ( "active", active ), ( "char", True ) ] ] [ text (String.fromChar char) ]


displayBufferChars : CursorPosition -> CursorPosition -> List Char -> List (Html msg)
displayBufferChars cursorPosition currentPosition chars =
    case chars of
        [] ->
            [ (displayBufferChar cursorPosition currentPosition ' ') ]

        x :: xs ->
            (displayBufferChar cursorPosition currentPosition x) :: (displayBufferChars cursorPosition { currentPosition | x = (currentPosition.x + 1) } xs)


displayBufferLine : CursorPosition -> Int -> String -> Html msg
displayBufferLine cursorPosition y line =
    div [ class "line" ] (displayBufferChars cursorPosition { x = 0, y = y } (String.toList line))


displayBufferLines : CursorPosition -> Int -> List String -> List (Html msg)
displayBufferLines cursorPosition y lines =
    case lines of
        [] ->
            []

        line :: xs ->
            (displayBufferLine cursorPosition y line) :: (displayBufferLines cursorPosition (y + 1) xs)


displayBuffer : Model -> List (Html msg)
displayBuffer model =
    displayBufferLines model.cursorPosition 0 model.buffer


view : Model -> Html Msg
view model =
    div []
        [ main_ [] (displayBuffer model)
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
