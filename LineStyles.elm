module LineStyles where

import Html exposing (Attribute)
import Html.Attributes exposing (..)

type alias LineStyle =
  { line    : Attribute
  , cursor  : Attribute
  , lineNum : Attribute
  } 

default : LineStyle
default =
  { line=lineStyle
  , cursor=underlineCursor
  , lineNum=lineNumStyle
  }

lineStyle : Attribute
lineStyle = style
  [ ("height", "1em")
  ]

solidCursor : Attribute
solidCursor = style
  [ ("background-color", "grey") 
  ]

underlineCursor : Attribute
underlineCursor = style
  [ ("text-decoration", "underline")
  ]

lineNumStyle = style
  [ ("display", "inline-block")
  , ("width", "2em")
  ]