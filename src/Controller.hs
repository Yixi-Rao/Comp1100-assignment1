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

      | k == "Backspace" || k == "Delete" -> undefined -- TODO: drop the last added shape
      | k == " " -> undefined -- TODO: finish polygon vertices
      | k == "T" -> undefined -- TODO: switch tool
      | k == "C" -> undefined -- TODO: switch colour

      -- ignore other events
      | otherwise -> m
      where k = unpack key
    PointerPress p -> undefined -- TODO
    PointerRelease p -> undefined -- TODO
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








