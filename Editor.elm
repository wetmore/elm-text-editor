module Editor where

import EditorActions exposing (..)
import TextBuffer as TB
import Buffer exposing (Line)
import LineStyles exposing (LineStyle)
import History exposing (..)

import Graphics.Element exposing (..)

import Html exposing (..)
import Signal exposing (..)
import Char
import Time exposing (..)
import Keyboard

import Debug

type EditType = Insert | Replace
type EditorMode = NormalMode | EditMode EditType 
type HBuffer = HBuffer TB.Model (History TB.Model)

-- MODEL

-- An editor holds a zipper of TextBuffer models (which are buffers of chars)
type alias Model = 
  { mode : EditorMode
  , buffer : HBuffer
  }

init : Model
init = normalMode <| HBuffer TB.oneLiner <| record TB.oneLiner <| emptyHistory 100 

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

normalMode : HBuffer -> Model
normalMode m = {mode=NormalMode, buffer=m}

insertMode : HBuffer -> Model
insertMode m = {mode=EditMode Insert, buffer=m}

update : Action -> Model -> Model
update (key, editorAction) {mode, buffer} = let
    (HBuffer buf hist) = buffer
    n = normalMode
    i = insertMode
    do x = let
        new = TB.applyActions x buf
      in HBuffer new (record new hist)
    doNoSave x = let
        new = TB.applyActions x buf
      in HBuffer new hist
  in case mode of -- 
    EditMode _ -> case key of -- Key -> TextBuffer
      Ret     -> i <| do [TB.Down]
      Esc     -> n buffer
      Press c -> i <| do [TB.Insert c]
    NormalMode -> case editorAction of  -- EditorAction -> TextBuffer
      C (EnterInsert UnderCursor) -> i buffer
      C (EnterInsert AfterCursor) -> i <| doNoSave [TB.Right]
      C (EnterInsert EOL)         -> i <| doNoSave [TB.EOL]
      C (EnterInsert BOL)         -> i <| doNoSave [TB.BOL]
      M Left                      -> n <| doNoSave [TB.Left]
      M Down                      -> n <| doNoSave [TB.Down]
      M Up                        -> n <| doNoSave [TB.Up]
      M Right                     -> n <| doNoSave [TB.Right]
      C (Remove Forward)          -> n <| do [TB.Delete]
      C (ReplaceChar c)           -> n <| do [TB.Delete, TB.Insert c, TB.Left]
      C (NewLine Below)           -> i <| do [TB.InsertLine, TB.Down]
      C (NewLine Above)           -> i <| do [TB.Up, TB.InsertLine] -- this is incorrect. What if we are at the top line?
      C SwapCase                  -> n <| do [TB.SwapCase, TB.Right]
      C Undo                      -> case recall hist of
                                       Nothing         -> n <| buffer
                                       Just (b, past)  -> n <| HBuffer b past
      _                           -> n buffer

-- VIEW

present (HBuffer b _) = b

view : Model -> Html
view {mode, buffer} = TB.viewWith (modeStyle mode) <| present buffer

modeStyle : EditorMode -> LineStyle
modeStyle mode = let
    defaultStyle = LineStyles.default
  in case mode of
    NormalMode -> defaultStyle
    EditMode _ -> { defaultStyle | cursor <- LineStyles.solidCursor }

watchHistory : Model -> Model
watchHistory m = let
    (HBuffer _ hist) = m.buffer
    _ = Debug.watch "History" hist
  in m 

-- CONTROL

{--
main : Signal Element
main = show <~ EditorActions.pairs
--}

{--}
main : Signal Html
main = view <~ model 
--}

-- There is a bug where the first state can't be undone
model : Signal Model
model = foldp update init (Debug.watch "Editor Actions" <~ EditorActions.signal)


