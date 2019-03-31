--- Copyright 2019 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> initialModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show m)) m

      -- display the mystery image
      | k == "M" -> Model mystery t c

      | k == "Backspace" || k == "Delete" -> Model (droplast ss) t c

      --  drop the last added shape
      | k == " " -> Model ss (PolygonTool []) c --  finish polygon vertices
      | k == "T" -> Model ss (nextTool t) c  --  switch tool
      | k == "C" -> Model ss t (nextColour c) --  switch colour
      -- ignore other events
      | otherwise -> m
      where k = unpack key
            droplast :: [a] -> [a]
            droplast list =case list of
                       []-> error "no elements in the list"
                       _:xs-> xs
    PointerPress p
        -- start at a point and Press the point with your current tool
        | t==LineTool Nothing -> Model [] (LineTool (Just p)) c
        | t==PolygonTool [] -> Model [] (PolygonTool (p:[])) c

        | t==PolygonTool (p:[p]) -> Model [] (PolygonTool ([p]++(p:[p]))) c
        | t==RectangleTool Nothing -> Model [] (RectangleTool (Just p)) c
        | t==CircleTool Nothing -> Model [] (CircleTool (Just p)) c
        | t==EllipseTool Nothing -> Model [] (EllipseTool (Just p)) c
    PointerRelease p
        -- ended at the point p,and added it into a model
        | t==LineTool (Just p)-> Model [(c,Line p p)] (LineTool Nothing) c
        | t==RectangleTool (Just p) -> Model [(c,Rectangle p p)] (RectangleTool Nothing) c
        | t==CircleTool (Just p) -> Model [(c,Circle p p)] (CircleTool Nothing) c
        | t==EllipseTool (Just p) -> Model [(c,Ellipse p p)] (EllipseTool Nothing) c


    _ -> m

-- move to the next colour
nextColour :: ColourName -> ColourName
nextColour colour = case colour of
    Black -> Red
    Red -> Orange
    Orange -> Yellow
    Yellow -> Green
    Green -> Blue
    Blue -> Violet
    Violet -> Black

-- This function implements tool-switching, but should not change Tool if the user is halfway through an operation
nextTool :: Tool -> Tool
nextTool tool = case tool of
   LineTool (Nothing) -> PolygonTool []
   LineTool (Just (a,b)) -> LineTool (Just (a,b))

   PolygonTool [] -> RectangleTool Nothing
   PolygonTool names -> PolygonTool names


   RectangleTool (Nothing) -> CircleTool Nothing
   RectangleTool (Just (a,b)) -> RectangleTool (Just (a,b))

   CircleTool (Nothing) -> EllipseTool Nothing
   CircleTool (Just (a,b)) -> CircleTool (Just (a,b))

   EllipseTool (Nothing) -> LineTool Nothing
   EllipseTool (Just (a,b)) -> EllipseTool (Just (a,b))








