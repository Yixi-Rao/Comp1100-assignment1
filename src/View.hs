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
colourNameToColour colour = case colour of
    Black -> black
    Red -> red
    Orange -> orange
    Yellow -> yellow
    Green -> green
    Blue -> blue
    Violet -> violet

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture shape = case shape of
    Line (a,b) (c,d)->(polyline [(a,b),(c,d)]) & coordinatePlane
    Polygon [(a,b)] -> solidPolygon [(a,b)]
    Rectangle (a,b) (c,d) -> (solidRectangle (abs (a-c)) (abs (b-d)))& coordinatePlane
    Circle (a,b) (c,d) -> (solidCircle (sqrt ((a-c)**2+(b-d)**2)))& coordinatePlane
    Ellipse (a,b) (c,d) -> solidCircle (sqrt ((a-c)**2+(b-d)**2))

