module TextBufferStyles
  ( lineStyle, lineNumStyle, bufferStyle, cursorStyle
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

lineNumStyle = style
  [ ("display", "inline-block")
  , ("width", "2em")
  ]