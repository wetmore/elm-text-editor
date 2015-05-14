module Buffer where
{-
  ( Line, Buffer, LineData
  , getLists
  , asList, asTaggedList, mapLine
  , goLeft, goRight
  , goUp, goDown
  , insertAtCursor, removeAtCursor
  , insertLine, emptyLine, emptyBuffer
  ) where
-}

import List exposing (..)

type Line a = Line (List a, List a) (Int, Int)
type alias LineData = { num : Int, current : Bool }

type alias Buffer a = Line (Line a)

isEmpty : Line a -> Bool
isEmpty l = (getLengths l) == (0,0)

atEnd : Line a -> Bool
atEnd (Line _ (n,_)) = n == 0

beginningOfLine : Buffer a -> Buffer a
beginningOfLine = atCurrentLine <| moveCursorTo 0

endOfLine : Buffer a -> Buffer a 
endOfLine = atCurrentLine (\l -> moveCursorTo (length l) l)

length : Line a -> Int
length (Line _ (n,m)) = n + m

getLists : Line a -> (List a, List a)
getLists (Line lists _) = lists

getLengths : Line a -> (Int, Int)
getLengths (Line _ len) = len

mapLine : (a -> b) -> (a -> b) -> Line a -> Line b
mapLine f g (Line (xs, bs) len) = Line (map f xs, map g bs) len

asList : Line a -> List a
asList (Line (xs, bs) _) = append (reverse bs) xs

tag : Int -> Bool -> Line a -> (LineData, Line a)
tag n b l = ({ num=n, current=b }, l)

asTaggedList : Buffer a -> List (LineData, Line a)
asTaggedList (Line (l::ls, bs) (n,m)) = let
    taggedls = (tag (m+1) True l) :: indexedMap (\i l' -> tag (i+m+2) False l') ls
    taggedbs = indexedMap (\i l' -> tag (m-i) False l') bs
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

moveCursorTo : Int -> Line a -> Line a
moveCursorTo i line = let
   (Line (xs, bs) (n,m)) = line
  in if
    | i > n + m -> Line ([], append (reverse xs) bs) (0, n + m)
    | i < 0     -> Line (append (reverse bs) xs, []) (n + m, 0)
    | i > m     -> moveCursorTo i (goRightL line)
    | i < m     -> moveCursorTo i (goLeftL line)
    | otherwise -> line

goUp : Buffer a -> Buffer a
goUp (Line lists (n,m)) = case lists of
  (ls, [])       -> Line (ls, []) (n,m)
  (l::ls, b::bs) -> let
      (_, i) = getLengths l
    in Line ((moveCursorTo i b)::l::ls, bs) (n+1, m-1)


goDown : Buffer a -> Buffer a
goDown (Line lists (n,m)) = case lists of
  ([], bs)  -> Line ([], bs) (n,m)
  ([l], bs) -> case (isEmpty l) of
    True  -> Line ([l], bs) (n,m)
    False -> insertLine emptyLine (Line ([], l::bs) (0, m+1))
  (l::l'::ls, bs) -> let
      (_, i) = getLengths l
    in Line ((moveCursorTo i l')::ls, l::bs) (n-1, m+1)

atCurrentLine : (Line a -> Line a) -> Buffer a -> Buffer a
atCurrentLine f (Line buf len) = case buf of
  ([], bs) -> Line ([], bs) len
  (l::ls, bs) -> Line (f l::ls, bs) len

goLeft : Buffer a -> Buffer a
goLeft = atCurrentLine goLeftL

goRight : Buffer a -> Buffer a
goRight = atCurrentLine goRightL

insertAtCursor : a -> Buffer a -> Buffer a
insertAtCursor x = atCurrentLine (insertInLine x)

removeAtCursor : Buffer a -> Buffer a
removeAtCursor = atCurrentLine removeInLine

insertLine : Line a -> Buffer a -> Buffer a
insertLine l (Line (ls, bs) (n,m)) = Line (l::ls, bs) (n+1, m)

removeInLine : Line a -> Line a
removeInLine (Line (xs, bs) (n,m)) = case xs of
  []    -> Line ([], bs) (n,m)
  x::xs -> Line (xs, bs) (n-1,m)

insertInLine : a -> Line a -> Line a
insertInLine x (Line (xs, bs) (n,m)) = Line (xs, x::bs) (n, m+1)

