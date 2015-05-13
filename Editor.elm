module Editor where

import TextBuffer as TB
import Buffer exposing (Line)
import LineStyles exposing (LineStyle)
import Html exposing (..)
import Signal exposing (..)
import Char

import Keyboard


type EditorMode = NormalMode | InsertMode

-- MODEL

-- An editor holds a zipper of TextBuffer models (which are buffers of chars)
type alias Model = 
  { mode : EditorMode
  , buffer : TB.Model
  }

init : Model
init = normalMode TB.oneLiner

{-
The editor should maintain a list of buffers, drawing the current one
and sending signals to it as well.

Signals have actions which are derived from the keypresses. 

In Normal mode, keys/combinations are turned into "editing" actions, like
remove char under cursor, go up, go down, etc.

In Insert mode, keys are mapped to insert char actions

Hence the concept of editor mode lives in the editor and is separate from
the textbuffer. The textbuffer handles its own rendering.
-}

-- UPDATE

type Action = Press Char | Esc

type MaybeInt = Nothing | Just Int


type BufferAction = MoveUp Int
                  | MoveDown Int
                  | MoveLeft Int
                  | MoveRight Int
                  | Insert Char

normalMode : TB.Model -> Model
normalMode m = {mode=NormalMode, buffer=m}

insertMode : TB.Model -> Model
insertMode m = {mode=InsertMode, buffer=m}

update : Action -> Model -> Model
update action {mode, buffer} = case action of
  Press c -> case mode of
    NormalMode -> case c of
      'i' -> insertMode buffer
      'h' -> normalMode <| TB.update (TB.Left) buffer
      'j' -> normalMode <| TB.update (TB.Down) buffer
      'k' -> normalMode <| TB.update (TB.Up) buffer
      'l' -> normalMode <| TB.update (TB.Right) buffer
      'x' -> normalMode <| TB.update (TB.Delete) buffer
      _  -> normalMode buffer
    InsertMode -> insertMode <| TB.update (TB.Insert c) buffer
  Esc -> normalMode buffer
  

-- VIEW

view : Model -> Html
view {mode, buffer} = TB.viewWith (modeStyle mode) buffer

modeStyle : EditorMode -> LineStyle
modeStyle mode = let
    defaultStyle = LineStyles.default
  in case mode of
    NormalMode -> defaultStyle
    InsertMode -> { defaultStyle | cursor <- LineStyles.solidCursor }

-- CONTROL

main : Signal Html
main = view <~ model 

model : Signal Model
model = Signal.foldp update init actions

pressChar : Keyboard.KeyCode -> Action
pressChar c = Press (Char.fromCode c)

actions : Signal Action
actions = merge esc <| pressChar <~ Keyboard.presses

esc : Signal Action
esc = sampleOn (Signal.filter (\x -> x) False <| Keyboard.isDown 27) (constant Esc)