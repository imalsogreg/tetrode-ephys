module Data.Ephys.PositionDecoding where

import Data.Ephys.PlaceCell
import Data.Ephys.TrackPosition

import qualified Data.Map as Map

estimatePosition :: [PlaceCell]              -- Cells & their plc fields
                 -> Map.Map PlaceCell Double -- Cells' binned spike counts
                 -> Field Double             -- Pos PDF
estimatePosition = undefined