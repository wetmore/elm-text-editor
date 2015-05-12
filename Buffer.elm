module Buffer
  ( Line, Buffer, LineData
  , getLists
  , asList, asTaggedList, mapLine
  , goLeft, goRight
  , goUp, goDown
  , insertAtCursor
  , insertLine, emptyLine, emptyBuffer
  ) where

import List exposing (..)

type Line a = Line (List a, List a) (Int, Int)
type alias Buffer a = Line (Line a)
type alias LineData = { current : Bool }

getLists : Line a -> (List a, List a)
getLists (Line lists _) = lists

mapLine : (a -> b) -> (a -> b) -> Line a -> Line b
mapLine f g (Line (xs, bs) len) = Line (map f xs, map g bs) len

asList : Line a -> List a
asList (Line (xs, bs) _) = append (reverse bs) xs

tag : Bool -> Line a -> (LineData, Line a)
tag b l = ({ current=b }, l)

asTaggedList : Buffer a -> List (LineData, Line a)
asTaggedList (Line (l::ls, bs) _) = let
    taggedls = (tag True l) :: map (tag False) ls
    taggedbs = map (tag False) bs
  in append (reverse taggedbs) taggedls

emptyLine : Line a
emptyLine = Line ([], []) (0, 0)

emptyBuffer : Buffer a
emptyBuffer = emptyLine

goLeftL : Line a -> Line a
goLeftL (Line lists (n,m)) = case lists of
  (xs, []) -> Line (xs, []) (n, m)
  (xs, b::bs) -> Line (b::xs, bs) (n+1, m-1)

goRightL : Line a -> Line a
goRightL (Line lists (n,m)) = case lists of
  ([], bs) -> Line ([], bs) (n,m)
  (x::xs, bs) -> Line (xs, x::bs) (n-1, m+1)

goUp : Buffer a -> Buffer a
goUp = goLeftL

goDown : Buffer a -> Buffer a
goDown (Line buf (n,m)) = case buf of
  ([], bs) -> Line ([], bs) (n,m)
  ([l], bs) -> Line ([l], bs) (n,m)
  (l::ls, bs) -> Line (ls, l::bs) (n-1, m+1)

goLeft : Buffer a -> Buffer a
goLeft (Line buf len) = case buf of
  ([], bs) -> Line ([], bs) len
  (l::ls, bs) -> Line ((goLeftL l)::ls, bs) len

goRight : Buffer a -> Buffer a
goRight (Line buf len) = case buf of
  ([], bs) -> Line ([], bs) len
  (l::ls, bs) -> Line ((goRightL l)::ls, bs) len

insertLine : Line a -> Buffer a -> Buffer a
insertLine l (Line (ls, bs) (n,m)) = Line (l::ls, bs) (n+1, m)

insertInLine : a -> Line a -> Line a
insertInLine x (Line (xs, bs) (n,m)) = Line (xs, x::bs) (n, m+1)

insertAtCursor : a -> Buffer a -> Buffer a
insertAtCursor x (Line buf len) = case buf of
  ([], bs) -> Line ([], bs) len
  (l::ls, bs) -> Line ((insertInLine x l)::ls, bs) len