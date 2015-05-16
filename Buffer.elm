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


-- LINE OPERATIONS
-- A `Line a` is a list of `a`s with the notion of a cursor. Operations around
-- the cursor should be constant time; moving the cursor from the nth element to
-- the n+1th element shouldn't require moving from the beginning of the list.
-- This is implemented by splitting the line into two lists (xs,bs), where xs
-- represents the elements remaining in the list and bs is a stack of elements
-- we've seen so far. For efficiency of certain operations we also store the
-- respective lengths of the lists in a tuple (n,m). Hence the list represented
-- by `Line (xs,bs) (n,m)` is the list `append (reverse bs) xs` (see the
-- `asList` function below), and it has length n+m.
-- 
-- By our convention, the cursor is positioned at the head of xs.

emptyLine : Line a
emptyLine = Line ([], []) (0, 0)

isEmpty : Line a -> Bool
isEmpty l = (getLengths l) == (0,0)

atEnd : Line a -> Bool
atEnd (Line _ (n,_)) = n == 0

length : Line a -> Int
length (Line _ (n,m)) = n + m

getLists : Line a -> (List a, List a)
getLists (Line lists _) = lists

getLengths : Line a -> (Int, Int)
getLengths (Line _ len) = len

-- Apply a function to the element under the cursor.
atCursor : (a -> a) -> Line a -> Line a
atCursor f (Line lists len) = case lists of
  ([], bs) -> Line ([], bs) len
  (x::xs, bs) -> Line (f x::xs, bs) len

asList : Line a -> List a
asList (Line (xs, bs) _) = append (reverse bs) xs

goLeftL : Line a -> Line a
goLeftL (Line lists (n,m)) = case lists of
  (xs, []) -> Line (xs, []) (n, m)
  (xs, b::bs) -> Line (b::xs, bs) (n+1, m-1)

goRightL : Line a -> Line a
goRightL (Line lists (n,m)) = case lists of
  ([], bs) -> Line ([], bs) (n,m)
  (x::xs, bs) -> Line (xs, x::bs) (n-1, m+1)

-- zero indexed
moveCursorTo : Int -> Line a -> Line a
moveCursorTo i line = let
   (Line (xs, bs) (n,m)) = line
  in if
    | i > n + m -> Line ([], append (reverse xs) bs) (0, n + m)
    | i < 0     -> Line (append (reverse bs) xs, []) (n + m, 0)
    | i > m     -> moveCursorTo i (goRightL line)
    | i < m     -> moveCursorTo i (goLeftL line)
    | otherwise -> line

-- Inserts at the cursor. Since the cursor is positioned at the head of xs,
-- the element is inserted and the cursor is moved to the next character.
insertInLine : a -> Line a -> Line a
insertInLine x (Line (xs, bs) (n,m)) = Line (xs, x::bs) (n, m+1)

-- Removes the element under the cursor.
removeInLine : Line a -> Line a
removeInLine (Line (xs, bs) (n,m)) = case xs of
  []    -> Line ([], bs) (n,m)
  x::xs -> Line (xs, bs) (n-1,m)


-- BUFFER OPERATIONS
-- Buffers are Lines of Lines; the current line is the head of the remaining
-- lines list, and the cursor position within this line is described above.

emptyBuffer : Buffer a
emptyBuffer = emptyLine

-- Note this assumes there is always at least one line in a buffer, so the
-- function is not actually total...........
-- It may be worth using a data structure which doesn't allow 0 lines in a
-- buffer. But then we can't use generic line operations on buffers...
goUp : Buffer a -> Buffer a
goUp (Line lists (n,m)) = case lists of
  (ls, [])       -> Line (ls, []) (n,m)
  (l::ls, b::bs) -> let
      (_, i) = getLengths l
    in Line ((moveCursorTo i b)::l::ls, bs) (n+1, m-1)

-- currently this inserts a new line if the last line is full. This is not
-- quite the behaviour vim has and should be fixed.
goDown : Buffer a -> Buffer a
goDown (Line lists (n,m)) = case lists of
  ([], bs)  -> Line ([], bs) (n,m)
  ([l], bs) -> case (isEmpty l) of
    True  -> Line ([l], bs) (n,m)
    False -> insertLine emptyLine (Line ([], l::bs) (0, m+1))
  (l::l'::ls, bs) -> let
      (_, i) = getLengths l
    in Line ((moveCursorTo i l')::ls, l::bs) (n-1, m+1)

beginningOfLine : Buffer a -> Buffer a
beginningOfLine = atCurrentLine <| moveCursorTo 0

endOfLine : Buffer a -> Buffer a 
endOfLine = atCurrentLine (\l -> moveCursorTo (length l) l)

-- Apply a function on Lines to the current line. 
atCurrentLine : (Line a -> Line a) -> Buffer a -> Buffer a
atCurrentLine = atCursor

goLeft : Buffer a -> Buffer a
goLeft = atCurrentLine goLeftL

goRight : Buffer a -> Buffer a
goRight = atCurrentLine goRightL

{-- might be worth finishing if more uses come along
modifyUnderCursor : (a -> List a) -> Buffer a -> Buffer a
modifyUnderCursor f buf = let
    modify l@(Line (xs,bs) (n,m)) = case xs of
      []    -> l
      x::xs -> let
          result = f x
        in Line (reverse 
  in atCurrentLine modify
--}

modifyUnderCursor : (a -> a) -> Buffer a -> Buffer a
modifyUnderCursor f = atCurrentLine <| atCursor f

insertAtCursor : a -> Buffer a -> Buffer a
insertAtCursor x = atCurrentLine <| insertInLine x

removeAtCursor : Buffer a -> Buffer a
removeAtCursor = atCurrentLine removeInLine

insertLine : Line a -> Buffer a -> Buffer a
insertLine l (Line (ls, bs) (n,m)) = Line (l::ls, bs) (n+1, m)

-- LINEDATA STUFF
-- This is used to annotate lines with information useful for the editor.
-- It should probably go in TextBuffer instead of here?

tag : Int -> Bool -> Line a -> (LineData, Line a)
tag n b l = ({ num=n, current=b }, l)

asTaggedList : Buffer a -> List (LineData, Line a)
asTaggedList (Line (l::ls, bs) (n,m)) = let
    taggedls = (tag (m+1) True l) :: indexedMap (\i l' -> tag (i+m+2) False l') ls
    taggedbs = indexedMap (\i l' -> tag (m-i) False l') bs
  in append (reverse taggedbs) taggedls

