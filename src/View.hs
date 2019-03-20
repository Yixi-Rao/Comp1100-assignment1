--- Copyright 2019 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a CodeWorld picture.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & colourShapesToPicture ss
  & coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText (toolLabel t)
    stringToText = text . pack

-- TODO
toolLabel :: Tool -> String
toolLabel x =  case x of
    LineTool _-> "Line... click-drag-release"
    PolygonTool _ -> "Polygon... click 3 or more times then spacebar"
    RectangleTool _ -> "Rectangle... click-drag-release"
    CircleTool _ -> "Circle... click-drag-release"
    EllipseTool _-> "Ellipse... click-drag-release"

-- TODO
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture = undefined

-- TODO
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture = undefined

-- TODO
colourNameToColour :: ColourName -> Colour
colourNameToColour = undefined

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture = undefined
