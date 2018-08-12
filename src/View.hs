--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld
import Data.Text (pack)
import Model

modelToPicture :: Model -> Picture
modelToPicture (Model ss t c) =
  translated 0 8 toolText & translated 0 7 colourText & colourShapesToPicture ss
  where
    colourText = stringToText (show c)
    toolText = stringToText toolString
    toolString =
      case t of
        LineTool _ -> "Line... click-drag-release"
        PolygonTool _ -> "Polygon... click 3 or more times then spacebar"
        RectangleTool _ -> "Rectangle ... click-drag-release"
        CircleTool _ -> "Circle... click-drag-release"
        EllipseTool _ -> "Ellipse... click-drag-release"
    stringToText = text . pack

-- TODO
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture = undefined

-- TODO
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (c, s) = undefined

-- TODO
colourNameToColour :: ColourName -> Colour
colourNameToColour c = undefined

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture s = undefined

{- Some example shapes -}
lineShape :: Shape
lineShape = Line (-1, -1) (1, 1)

squareShape :: Shape
squareShape = Rectangle (0, 0) (5, 5)

rectangleShape :: Shape
rectangleShape = Rectangle (-5, -2) (5, 1)

triangleShape :: Shape
triangleShape = Polygon [(0, 5), (-5, -5), (5, -5)]

circleShape :: Shape
circleShape = Circle (0, 0) (5, 0)

ellipseShape :: Shape
ellipseShape = Ellipse (-5, -1) (5, 1)

redLineShape :: ColourShape
redLineShape = (Red, lineShape)

mystery :: Model
mystery =
  Model
    [ ( Black
      , Polygon
          [ (-0.8816705336426914, 0.951276102088167)
          , (-0.8352668213457076, -1.8329466357308584)
          , (-1.1832946635730859, -1.8329466357308584)
          , (-1.0672853828306264, 1.136890951276102)
          ])
    , ( Black
      , Polygon
          [ (-1.4617169373549883, 1.1832946635730859)
          , (-1.5081206496519721, -1.8793503480278422)
          , (-1.8793503480278422, -1.8793503480278422)
          , (-1.902552204176334, 0.8584686774941995)
          ])
    , ( Black
      , Line
          (-2.482598607888631, 3.109048723897912)
          (-3.1554524361948957, 3.109048723897912))
    , ( Black
      , Line
          (-2.3897911832946637, 2.9466357308584685)
          (-3.08584686774942, 2.7842227378190256))
    , ( Black
      , Line
          (-2.34338747099768, 3.225058004640371)
          (-3.3178654292343386, 3.3874709976798143))
    , ( Black
      , Line
          (-0.46403712296983757, 2.807424593967517)
          (-0.11600928074245939, 2.645011600928074))
    , ( Black
      , Line
          (-0.39443155452436196, 2.9930394431554523)
          (0.30162412993039445, 2.830626450116009))
    , ( Black
      , Line
          (-0.5104408352668214, 3.1322505800464038)
          (0.4176334106728538, 3.248259860788863))
    , ( Yellow
      , Line
          (-1.4385150812064964, 2.691415313225058)
          (-1.136890951276102, 2.830626450116009))
    , ( Yellow
      , Line
          (-1.716937354988399, 2.7610208816705337)
          (-1.4385150812064964, 2.691415313225058))
    , ( Yellow
      , Line
          (-1.4153132250580045, 3.1554524361948957)
          (-1.4153132250580045, 2.6218097447795823))
    , ( Yellow
      , Circle
          (-0.974477958236659, 3.573085846867749)
          (-0.8584686774941995, 3.596287703016241))
    , ( Yellow
      , Circle
          (-1.8561484918793503, 3.642691415313225)
          (-1.716937354988399, 3.5498839907192576))
    , ( Orange
      , Polygon
          [ (-0.27842227378190254, -1.3921113689095128)
          , (1.2761020881670533, -0.3480278422273782)
          , (-0.13921113689095127, -1.0208816705336428)
          ])
    , ( Orange
      , Circle
          (-1.368909512761021, 3.201856148491879)
          (-0.16241299303944315, 2.877030162412993))
    , ( Orange
      , Ellipse
          (-2.668213457076566, 2.34338747099768)
          (0.18561484918793503, -2.1345707656612527))
    ]
    (PolygonTool [])
    Black
