module History
  ( History, record, recall, withNewLimit, emptyHistory
  ) where

import List exposing (reverse, tail)

type alias History a = { past : List a, size : Int, limit : Int}

emptyHistory : Int -> History a
emptyHistory n = { past=[], size=0, limit=n }

cotail : List a -> List a
cotail list = case list of
  x::y::xs -> x::y::cotail xs
  _        -> []

record : a -> History a -> History a
record event {past, size, limit} = case size < limit of
  True  -> {past=event::past, size=size+1, limit=limit}
  False -> {past=event::(cotail past), size=size, limit=limit} 

recall : History a -> Maybe (a, History a)
recall {past, size, limit} = case past of
  []    -> Nothing
  x::xs -> Just (x, {past=xs, size=size-1, limit=limit})

forget : History a -> History a
forget hist = case hist.past of
  []    -> hist
  x::xs -> { hist | past <- xs, size <- hist.size-1}

withNewLimit : Int -> History a -> History a
withNewLimit n history = { history | limit <- n}