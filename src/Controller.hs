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

-- TODO
nextColour :: ColourName -> ColourName
nextColour = undefined

-- TODO
nextTool :: Tool -> Tool
nextTool = undefined
