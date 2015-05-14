# Elm Text Editor

Simple modal editor to play with elm and zippers. Currently supports normal
and insert modes with few features. 

## Todo

- Window for current buffer, which resizes with window and has ~s
- Continue implementing the editor actions I capture from the keyboard
- Status line, : commands

## Goals

- modes (insert, normal, visual)
- save/load
- undo/redo
- decent feature parity with vi
- vim's text objects, like `iw` used with certain operators as in `ciw`
- Multiple open buffers

## Ideas

- Structural editing (S exps, etc)