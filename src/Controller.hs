--- Copyright 2018 The Australian National University, All rights reserved
module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

handleTime :: Double -> Model -> Model
handleTime = flip const

handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c) =
  case event of
    KeyPress key
      | k == "Esc" -> initialModel
        -- revert to an empty canvas
      | k == "D" -> trace (pack (show m)) m
        --   the current model on the console
      | k == "Backspace" || k == "Delete" -> undefined -- TODO: drop the last added shape
      | k == " " -> undefined -- TODO: finish polygon vertices
      | k == "T" -> undefined -- TODO: switch tool
      | k == "C" -> undefined -- TODO: switch colour
      | otherwise -> m -- ignore other events
      where k = unpack key
    MousePress LeftButton -> undefined -- TODO
    MouseRelease LeftButton -> undefined -- TODO
    _ -> m
