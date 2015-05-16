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

import Debug

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
update (key, editorAction) {mode, buffer} = let
    n = normalMode
    i = insertMode
    do x = TB.applyActions x buffer
  in case mode of
    EditMode _ -> case key of
      Ret     -> i <| do [TB.Down]
      Esc     -> n buffer
      Press c -> i <| do [TB.Insert c]
    NormalMode -> case editorAction of 
      C (EnterInsert UnderCursor) -> i buffer
      C (EnterInsert AfterCursor) -> i <| do [TB.Right]
      C (EnterInsert EOL)         -> i <| do [TB.EOL]
      C (EnterInsert BOL)         -> i <| do [TB.BOL]
      M Left                      -> n <| do [TB.Left]
      M Down                      -> n <| do [TB.Down]
      M Up                        -> n <| do [TB.Up]
      M Right                     -> n <| do [TB.Right]
      C (Remove Forward)          -> n <| do [TB.Delete]
      C (ReplaceChar c)           -> n <| do [TB.Delete, TB.Insert c, TB.Left]
      C (NewLine Below)           -> i <| do [TB.InsertLine, TB.Down]
      C (NewLine Above)           -> i <| do [TB.Up, TB.InsertLine] -- this is incorrect. What if we are at the top line?
      _                           -> n buffer 

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
model = foldp update init (Debug.watch "Editor Actions" <~ EditorActions.signal)


