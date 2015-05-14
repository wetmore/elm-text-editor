module Editor where

import EditorActions exposing (..)
import TextBuffer as TB
import Buffer exposing (Line)
import LineStyles exposing (LineStyle)

import Html exposing (..)
import Signal exposing (..)
import Char
import Time exposing (..)

import Keyboard

type EditType = Insert | Replace
type EditorMode = NormalMode | EditMode EditType 

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

type alias Action = (Key, EditorAction)

normalMode : TB.Model -> Model
normalMode m = {mode=NormalMode, buffer=m}

insertMode : TB.Model -> Model
insertMode m = {mode=EditMode Insert, buffer=m}

update : Action -> Model -> Model
update (key, editorAction) {mode, buffer} = case mode of
  EditMode _ -> case key of
    Ret     -> insertMode <| TB.update (TB.Down) buffer
    Esc     -> normalMode buffer
    Press c -> insertMode <| TB.update (TB.Insert c) buffer
  NormalMode -> case editorAction of 
    C (EnterInsert UnderCursor) -> insertMode buffer
    C (EnterInsert AfterCursor) -> insertMode <| TB.update (TB.Right) buffer
    C (EnterInsert EOL) -> insertMode <| TB.update (TB.EOL) buffer
    C (EnterInsert BOL) -> insertMode <| TB.update (TB.BOL) buffer
    M Left  -> normalMode <| TB.update (TB.Left) buffer
    M Down  -> normalMode <| TB.update (TB.Down) buffer
    M Up    -> normalMode <| TB.update (TB.Up) buffer
    M Right -> normalMode <| TB.update (TB.Right) buffer
    C (Remove Forward) -> normalMode <| TB.update (TB.Delete) buffer
    _   -> normalMode buffer 

-- VIEW

view : Model -> Html
view {mode, buffer} = TB.viewWith (modeStyle mode) buffer

modeStyle : EditorMode -> LineStyle
modeStyle mode = let
    defaultStyle = LineStyles.default
  in case mode of
    NormalMode -> defaultStyle
    EditMode _ -> { defaultStyle | cursor <- LineStyles.solidCursor }

-- CONTROL

main : Signal Html
main = view <~ model 

model : Signal Model
model = foldp update init actions

-- this is a bad way to do this
-- is it possible that while in insert mode we build a combo that executes?
actions : Signal Action
actions = (,) <~ EditorActions.keys ~ EditorActions.signal

