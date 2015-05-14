module EditorActions
  ( EditorAction(..), Motion(..), Command(..)
  , CursorPos(..), Direction(..), Vertical(..)
  , signal, Key(..), keys
  ) where

import Keyboard
import Signal exposing ((<~))
import Char
import Graphics.Element exposing (show, Element)
import String exposing (..)
import Result exposing (Result)
import Debug

-- EXTERNAL

type EditorAction = Count Int EditorAction | C Command | M Motion | Cancel

type CursorPos = UnderCursor | AfterCursor | BOL | EOL
type Direction = Forward | Backward
type Vertical = Above | Below

type Motion = Up | Down | Left | Right
            | BeginningOfLine { nonBlank : Bool }
            | EndOfLine
            | RepeatFind
            | ReverseFind
            | GotoLine
            | Find {dir : Direction, til : Bool} Char
            | LineUp | LineDown

type Command = EnterInsert CursorPos
             | Delete Motion
             | Remove Direction
             | NewLine Vertical
             | ReplaceChar Char

type Key = Press Char | Esc


-- INTERNAL

-- represents partially or fully completed key combinations.
-- partial combos carry around a function f which is applied to the resulting
-- EditorAction. We use this to wrap actions in Counts, or to handle
-- commands which operate on motions
type ComboState = Completed EditorAction
                | Partial (EditorAction -> EditorAction) (Key -> ComboState)
                | Null

idPartial : (Key -> ComboState) -> ComboState
idPartial f = Partial identity f

replaceUnderCursor : Key -> ComboState
replaceUnderCursor key = case key of
  Esc     -> Null 
  Press c -> Completed <| C <| ReplaceChar c

findChar : {dir : Direction, til : Bool} -> Key -> ComboState
findChar params key = case key of
  Esc     -> Null
  Press c -> Completed <| M <| Find params c

buildCount : String -> Key -> ComboState
buildCount s key = case key of
  Esc -> Null
  Press d -> case (Char.isDigit d) of
    True  -> idPartial <| buildCount (cons d s)
    False -> let
        possibleCount = toInt (reverse s)
      in case possibleCount of
        Err s -> Debug.log s Null
        Ok  n -> case (bindings d) of
          Completed action -> Completed <| Count n action
          Partial f g -> Partial ((Count n) << f) g
          Null -> Null

handleKey : Key -> ComboState -> ComboState
handleKey key state = case state of
  Partial f g -> case (g key) of
    Completed action -> Completed <| f action
    _                -> g key
  _         -> case key of
    Esc     -> Completed Cancel
    Press c -> case (Char.isDigit c) of
      True  -> case (c == '0') of
        True  -> bindings c
        False -> idPartial <| buildCount (fromChar c)
      False -> bindings c

bindings : Char -> ComboState
bindings c = let
    cc x = Completed (C x)
    cm x = Completed (M x)
  in case c of
    -- commands
    'a' -> cc <| EnterInsert AfterCursor
    'A' -> cc <| EnterInsert EOL
    'i' -> cc <| EnterInsert UnderCursor
    'I' -> cc <| EnterInsert BOL
    'o' -> cc <| NewLine Below
    'O' -> cc <| NewLine Above
    'x' -> cc <| Remove Forward
    'X' -> cc <| Remove Backward
    'r' -> idPartial replaceUnderCursor
    -- motions
    'h' -> cm <| Left
    'j' -> cm <| Down
    'k' -> cm <| Up
    'l' -> cm <| Right
    '0' -> cm <| BeginningOfLine {nonBlank=False}
    '^' -> cm <| BeginningOfLine {nonBlank=True}
    '$' -> cm <| EndOfLine
    ';' -> cm <| RepeatFind
    ',' -> cm <| ReverseFind
    '-' -> cm <| LineUp
    '+' -> cm <| LineDown
    'G' -> cm <| GotoLine
    'f' -> idPartial <| findChar {dir=Forward, til=False}
    'F' -> idPartial <| findChar {dir=Backward, til=False}
    't' -> idPartial <| findChar {dir=Forward, til=True}
    'T' -> idPartial <| findChar {dir=Backward, til=True}
    _   -> Null

comboState : Signal ComboState
comboState = Signal.foldp handleKey Null keys

completedCombo : ComboState -> Maybe EditorAction
completedCombo state = case state of
  Completed act -> Just act
  _             -> Nothing

signal : Signal EditorAction
signal = Signal.filterMap completedCombo Cancel comboState

pressChar : Keyboard.KeyCode -> Key
pressChar c = Press (Char.fromCode c)

keys : Signal Key
keys = Signal.merge esc <| pressChar <~ Keyboard.presses

esc : Signal Key
esc = let
    escDown = (Signal.filter identity False <| Keyboard.isDown 27) 
  in Signal.sampleOn escDown (Signal.constant Esc)