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

-- When you type some tool,and will return the method of how to use it
toolLabel :: Tool -> String
toolLabel x =  case x of
    LineTool _-> "Line... click-drag-release"
    PolygonTool _ -> "Polygon... click 3 or more times then spacebar"
    RectangleTool _ -> "Rectangle... click-drag-release"
    CircleTool _ -> "Circle... click-drag-release"
    EllipseTool _-> "Ellipse... click-drag-release"

-- This takes a list of a pair of colour and shape and integrates them into coordinatePlane
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture list = case list of
    [] -> blank
    [x] -> colourShapeToPicture x
    x:xs -> ((colourShapeToPicture x)&colourShapesToPicture xs)

-- This function takes a pair of colour and shape and fill some colour that you want into the shape
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (colourName, shape) =coordinatePlane&(coloured (colourNameToColour colourName) (shapeToPicture shape))

-- This takes a colour and turn it into codeworld colour
colourNameToColour :: ColourName -> Colour
colourNameToColour colour = case colour of
    Black -> black
    Red -> red
    Orange -> orange
    Yellow -> yellow
    Green -> green
    Blue -> blue
    Violet -> violet

-- It is a function that takes a shape and turn it into a solid picture or line in the coordinatePlane
shapeToPicture :: Shape -> Picture
shapeToPicture shape = case shape of
    Line (a,b) (c,d)->(polyline [(a,b),(c,d)]) & coordinatePlane

    Polygon (f:s:t:xs) -> (solidPolygon (f:s:t:xs)) & coordinatePlane
    Polygon [_, _] -> error "just two points,not enough!"
    Polygon [_] -> error "just one point,not enough!"
    Polygon [] -> error "no points in the list"

    Rectangle (a,b) (c,d) -> (translated (abs ((a+c)/2)) (abs ((b+d)/2)) (solidRectangle (abs (a-c)) (abs (b-d))))& coordinatePlane

    Circle (a,b) (c,d) -> (translated a b (solidCircle r))& coordinatePlane
      where r=sqrt ((a-c)**2+(b-d)**2)
      -- because r equals to the distance between the centre (a,b) and a point on the circle
    Ellipse (a,b) (c,d) -> (translate (scaled scalex scaley (solidCircle r)))& coordinatePlane

      where translate :: Picture -> Picture
            translate x = translated (abs ((a+c)/2)) (abs ((b+d)/2)) x
            --function that translate a scaled picture with (a+c)/2 and (b+d)/2
            r=(sqrt ((a-c)**2+(b-d)**2))/2
            scalex=(abs(a-c))/(2*r)
            scaley=(abs(b-d))/(2*r)


