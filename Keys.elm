module Keys where

import Keyboard exposing (KeyCode)
import Signal exposing ((<~), (~), map2)
import Set
import List
import Graphics.Element exposing (..)

import String
import Char

type Meta = Ctrl | Alt
type Key  = Press Char | Esc | Ret | Chord (List Meta) Key


metas = [16, 17, 18]

isMeta : KeyCode -> Bool
isMeta x = x `List.member` metas 

emptyPair = (Set.empty, Set.empty)

keys' = let
    fn (w,x) (_,z) = (w, x `Set.diff` z)
  in Signal.foldp fn emptyPair <| Set.partition isMeta <~ Keyboard.keysDown

switch : Signal Bool -> Signal a -> Signal a -> Signal a
switch b s s' = map2 (\t (a,a') -> if t then a else a') b <| (,) <~ s ~ s' -- triggers too many events

keys = Signal.filter ((/=) emptyPair) emptyPair keys'

metaDown (set, _) = List.length (Set.toList set) > 0

s = switch (Signal.map metaDown keys) keys (Signal.map (\k -> (Set.empty, Set.singleton k)) Keyboard.presses)

main : Signal Element
main = (\x -> flow down (List.map show x)) <~ (Signal.foldp (::) [] s)

draw list = let
    letters = List.map (\(_,x) -> List.map Char.fromCode <| Set.toList x) list
    string = String.concat <| List.map String.fromList letters
  in show (String.reverse string) 