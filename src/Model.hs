--- Copyright 2018 The Australian National University, All rights reserved
module Model where

type Coords = (Double, Double)

data Shape
  = Line Coords
         Coords
  | Polygon [Coords]
  | Rectangle Coords
              Coords
  | Circle Coords
           Coords
  | Ellipse Coords
            Coords
  deriving (Show)

type ColourShape = (ColourName, Shape)

data Tool
  = LineTool (Maybe Coords)
  | PolygonTool [Coords]
  | RectangleTool (Maybe Coords)
  | CircleTool (Maybe Coords)
  | EllipseTool (Maybe Coords)
  deriving (Show)

data ColourName
  = Black
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  deriving (Show)

data Model =
  Model [ColourShape]
        Tool
        ColourName
  deriving (Show)

initialModel :: Model
initialModel = Model [] (LineTool Nothing) Black
