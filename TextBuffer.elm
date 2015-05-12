module TextBuffer where

import Buffer exposing (..)
import Char
import Signal exposing ((<~))
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Graphics.Element exposing (Element, show, flow, down)
import Keyboard
import List exposing (map, reverse, foldr, append)
import String exposing (cons)

-- MODEL

type alias Model = Buffer Char

-- UPDATE

type Action = Up | Down | Left | Right | Insert Char | Noop

insertChar : Char -> Action
insertChar c = Insert c

{--}
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
  Noop -> model


-- VIEW

view : Model -> Html
view m = div [bufferStyle] (map showLine (asTaggedList m))

toString : List Char -> String
toString cs = String.concat (map String.fromChar cs)

showLine : (LineData, Line Char) -> Html
showLine ({current}, line) = let
    (xs, bs) = getLists line
    left = text << toString <| reverse bs
    rest = if
      | not current -> [text (toString xs)]
      | otherwise   -> case xs of
          []      -> [span [cursorStyle] [text "_"]]
          (y::ys) -> [span [cursorStyle] [text (String.fromChar y)], text (toString ys)]
  in div [] (left::rest)

{--}
bufferStyle : Attribute
bufferStyle = style
  [ ("font-family", "monospace")
  , ("padding", "20px 20px")
  ]
--}

cursorStyle : Attribute
cursorStyle = style
  [ ("background-color", "grey") 
  ]

main : Signal Html
main = view <~ model 

model : Signal Model
model = Signal.foldp update (insertLine emptyLine (insertLine emptyLine emptyBuffer)) actions

actions : Signal Action
actions = let
    keys = (insertChar << Char.fromCode) <~ Keyboard.presses
    arrs = arrToAction <~ Keyboard.arrows
  in Signal.merge keys arrs