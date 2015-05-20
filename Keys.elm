module Keys where

import Keyboard exposing (KeyCode)
import Signal exposing ((<~), (~))
import Set
import List
import Graphics.Element exposing (..)

type Meta = Ctrl | Alt
type Key  = Press Char | Esc | Ret | Chord (List Meta) Key


metas = [16, 17, 18]

isMeta : KeyCode -> Bool
isMeta x = x `List.member` metas 

emptyPair = (Set.empty, Set.empty)

keys' = let
    fn (w,x) (_,z) = (w, x `Set.diff` z)
  in Signal.foldp fn emptyPair <| Set.partition isMeta <~ Keyboard.keysDown

keys = Signal.filter ((/=) emptyPair) emptyPair keys'

main : Signal Element
main = (\x -> flow down (List.map show x)) <~ (Signal.foldp (::) [] keys)