module Buffer
  ( Buffer, asList
  , goLeft, goRight
  , goUp, goDown
  , insertAtCursor
  , insertLine, emptyLine, emptyBuffer
  ) where

import List exposing (..)

type alias Line a = (List a, List a)
type alias Buffer a = Line (Line a)

asList : Line a -> List a
asList (xs, bs) = append (reverse bs) xs

emptyLine : Line a
emptyLine = ([], [])

emptyBuffer : Buffer a
emptyBuffer = emptyLine

goLeftL : Line a -> Line a
goLeftL line = case line of
  (xs, []) -> (xs, [])
  (xs, b::bs) -> (b::xs, bs)

goRightL : Line a -> Line a
goRightL line = case line of
  ([], bs) -> ([], bs)
  ([x], bs) -> ([x], bs)
  (x::xs, bs) -> (xs, x::bs)

goUp : Buffer a -> Buffer a
goUp = goLeftL

goDown : Buffer a -> Buffer a
goDown = goRightL

goLeft : Buffer a -> Buffer a
goLeft buf = case buf of
  ([], bs) -> ([], bs)
  (l::ls, bs) -> ((goLeftL l)::ls, bs)

goRight : Buffer a -> Buffer a
goRight buf = case buf of
  ([], bs) -> ([], bs)
  (l::ls, bs) -> ((goRightL l)::ls, bs)

insertLine : Line a -> Buffer a -> Buffer a
insertLine l (ls, bs) = (l::ls, bs)

insertInLine : a -> Line a -> Line a
insertInLine x (xs, bs) = (xs, x::bs)

insertAtCursor : a -> Buffer a -> Buffer a
insertAtCursor x buf = case buf of
  ([], bs) -> ([], bs)
  (l::ls, bs) -> ((insertInLine x l)::ls, bs)