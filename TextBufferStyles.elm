module TextBufferStyles
  ( bufferStyle
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)

bufferStyle : Attribute
bufferStyle = style
  [ ("font-family", "monospace")
  --, ("padding", "20px 20px")
  ]