module TextBuffer where

import TextBufferStyles exposing (..)
import LineStyles exposing (LineStyle)
import Buffer exposing (..)
import Char
import Signal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Graphics.Element exposing (Element, show, flow, down)
import Time exposing (fpsWhen, since, Time, millisecond)
import List exposing (reverse, foldr, append)
import String exposing (cons, fromList)

-- MODEL

type alias Model = Buffer Char

oneLiner : Model
oneLiner = insertLine emptyLine emptyBuffer

-- UPDATE

type Action = Up | Down | Left | Right | Insert Char | Noop | Delete

{--
arrToAction : { x : Int, y : Int } -> Action
arrToAction {x, y} = if
  | x == -1 -> Left
  | x ==  1 -> Right
  | y == -1 -> Down
  | y ==  1 -> Up
  | otherwise -> Noop
--}

update : Action -> Model -> Model
update action model = case action of
  Up -> goUp model
  Down -> goDown model
  Left -> goLeft model
  Right -> goRight model
  Insert c -> insertAtCursor c model
  Delete -> removeAtCursor model
  Noop -> model


-- VIEW

view : Model -> Html
view = viewWith LineStyles.default

viewWith : LineStyle -> Model -> Html
viewWith sty m = div [bufferStyle] (List.map (showLineWith sty) (asTaggedList m))

showLineWith : LineStyle -> (LineData, Line Char) -> Html
showLineWith sty ({num, current}, line) = let
    (xs, bs) = getLists line
    left = text << fromList <| reverse bs
    rest = if
      | not current -> [text (fromList xs)]
      | otherwise   -> case xs of
          []      -> [span [sty.cursor] [text " "]]
          (y::ys) -> [span [sty.cursor] [text << String.fromChar <| y], text << fromList <| ys]
  in div [sty.line] <| (span [sty.lineNum] [text (toString num)])::left::rest

-- CONTROL

{--
repeatAfterIf : Time -> number -> (a -> Bool) -> Signal a -> Signal a
repeatAfterIf time fps predicate s =
  let repeatable = predicate <~ s
      delayedRep = repeatable |> filter identity False |> since time |> Signal.map not
      resetDelay = merge (always False <~ s) delayedRep
      repeats = fpsWhen fps <| (&&) <~ repeatable ~ (dropRepeats resetDelay)
  in sampleOn repeats s

repeatAfterMs : Int -> Signal a -> Signal a
repeatAfterMs n s = repeatAfterIf (toFloat n * millisecond) 30 (always True) s 
--}
