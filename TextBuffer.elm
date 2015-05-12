module TextBuffer where

import Buffer exposing (..)
import Char
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show, flow, down)
import Keyboard
import List exposing (map)

-- MODEL

type alias Model = Buffer Char

-- UPDATE

type Action = Up | Down | Left | Right | Insert Char | Noop

insertChar : Char -> Action
insertChar c = Insert c

{--
arrToAction : { x : Int, y : Int } -> Action
arrToAction arr = case arr of
  { x =  0, y = 0 } -> Noop
  { x = -1, y = 0 } -> Left
  { x = -1, y = 0 }
--}

update : Action -> Model -> Model
update action model = case action of
  Up -> goUp model
  Down -> goDown model
  Left -> goLeft model
  Right -> goRight model
  Insert c -> insertAtCursor c model
  Noop -> model


-- VIEW

view : Model -> Element
view m = let
    bufferArray = map asList (asList m)
  in flow down (map show bufferArray)


main : Signal Element
main = view <~ model 

model : Signal Model
model = Signal.foldp update (insertLine emptyLine (insertLine emptyLine emptyBuffer)) actions

actions : Signal Action
actions = let
    keys = (insertChar << Char.fromCode) <~ Keyboard.presses
  in keys