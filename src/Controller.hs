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

      | k == "Backspace" || k == "Delete" -> case ss of
                        [] -> Model [] t c
                        _:xs -> Model xs t c

      --  drop the last added shape
      | k == " " ->helperspace t
          --  finish polygon vertices
      | k == "T" -> Model ss (nextTool t) c  --  switch tool
      | k == "C" -> Model ss t (nextColour c) --  switch colour
      -- ignore other events
      | otherwise -> m
      where k = unpack key
            helperspace::Tool->Model
            helperspace tool = case tool of
                PolygonTool (a:b:l)-> Model [(c,Polygon (a:b:l))] (PolygonTool []) c
                PolygonTool (_:_)-> error "you just click some points,but not enough,we need 3 points at lease"
                _-> error "you use a wrong tool or it is a empty list, space key is designed to polygan"

    PointerPress p -> helperpress t
        -- start at a point and Press the point with your current tool
            where  helperpress::Tool -> Model
                   helperpress tool =case tool of
                       LineTool _ -> Model [] (LineTool (Just p)) c

                       PolygonTool []-> Model [] (PolygonTool [p]) c
                       PolygonTool [l]-> Model [] (PolygonTool (p:[l])) c
                       PolygonTool (a:b:l) -> Model [] (PolygonTool (p:a:b:l)) c

                       RectangleTool _ -> Model [] (RectangleTool (Just p)) c

                       CircleTool _ -> Model [] (CircleTool (Just p)) c

                       EllipseTool _ -> Model [] (EllipseTool (Just p)) c
    PointerRelease p -> case t of
            LineTool (Just q)-> Model [(c,Line p q)] (LineTool Nothing) c
            LineTool Nothing -> error "go back to click some points"

            RectangleTool (Just q) -> Model [(c,Rectangle p q)] (RectangleTool Nothing) c
            RectangleTool Nothing -> error "go back to click some points"

            CircleTool (Just q) -> Model [(c,Circle p q)] (CircleTool Nothing) c
            CircleTool Nothing -> error "go back to click some points"

            EllipseTool (Just q) -> Model [(c,Ellipse p q)] (EllipseTool Nothing) c
            EllipseTool Nothing -> error "go back to click some points"
            _ -> m
        -- ended at the point p,and added it into a model




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








