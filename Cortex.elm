module Cortex exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Char exposing (fromCode)
import Keyboard
import Regex


-- MODEL


type alias Model =
    { buffer : List String
    , cursorPosition : CursorPosition
    , shift : Bool
    }


type alias CursorPosition =
    { x : Int
    , y : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { buffer = [ "" ], cursorPosition = { x = 0, y = 0 }, shift = False }, Cmd.none )



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


-- TODO: Should also work for other char positions than the last one
insertNewline : Model -> Model
insertNewline model =
    let
        newBuffer =
            List.concat [ model.buffer, [ "" ] ]
    in
        { model | buffer = newBuffer, cursorPosition = { x = 0, y = model.cursorPosition.y + 1 } }


-- TODO: Should also work for other char positions than the last one
insertChar : Char -> Model -> Model
insertChar char model =
    let
        converted =
            if model.shift then
                String.fromChar (Char.toUpper char)
            else
                String.fromChar (Char.toLower char)

        newBuffer =
            insertAtLine model.cursorPosition.y 0 converted model.buffer
    in
        { model | buffer = newBuffer, cursorPosition = { x = model.cursorPosition.x + 1, y = model.cursorPosition.y }, shift = False }


-- TODO: Clamp (http://package.elm-lang.org/packages/elm-lang/core/5.0.0/Basics#clamp)
moveCursor : CursorPosition -> Model -> Model
moveCursor cursorPositionChange model =
    { model | cursorPosition = { x = model.cursorPosition.x + cursorPositionChange.x, y = model.cursorPosition.y + cursorPositionChange.y } }


shift : Model -> Model
shift model =
    { model | shift = True }


shouldBeInserted : Char.KeyCode -> Bool
shouldBeInserted x =
    x |> fromCode |> String.fromChar |> (Regex.contains (Regex.regex "^[A-Za-z0-9 ]$"))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Presses code ->
            case code of
                -- Enter
                13 ->
                    ( insertNewline model, Cmd.none )

                16 ->
                    ( shift model, Cmd.none )

                -- Left
                37 ->
                    ( moveCursor { x = -1, y = 0 } model, Cmd.none )

                -- Up
                38 ->
                    ( moveCursor { x = 0, y = -1 } model, Cmd.none )

                -- Right
                39 ->
                    ( moveCursor { x = 1, y = 0 } model, Cmd.none )

                -- Down
                40 ->
                    ( moveCursor { x = 0, y = 1 } model, Cmd.none )

                -- Other Chars
                _ ->
                    if shouldBeInserted code then
                        ( insertChar (fromCode code) model, Cmd.none )
                    else
                        ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Keyboard.presses does what I want, but does not work with arrows... and does not work in Firefox oO
    -- This results in the weird 'handle shift yourself' thing, which of course does not work outside of the
    -- standard chars...
    Keyboard.downs (\code -> Presses code)



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
