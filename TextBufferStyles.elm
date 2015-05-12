module TextBufferStyles
  ( lineStyle, bufferStyle, cursorStyle
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)

lineStyle : Attribute
lineStyle = style
  [ ("height", "1em")
  ]

bufferStyle : Attribute
bufferStyle = style
  [ ("font-family", "monospace")
  --, ("padding", "20px 20px")
  ]

cursorStyle : Attribute
cursorStyle = style
  [ ("background-color", "grey") 
  ]