module TextBufferStyles
  ( bufferStyle
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)

bufferStyle : Attribute
bufferStyle = style
  [ ("font-family", "monospace")
  , ("white-space", "pre")
  ]