module Data.Ephys.PositionDecoding where

import Data.Ephys.PlaceCell
import Data.Ephys.TrackPosition

import qualified Data.Map as Map


estimatePosition :: Map.Map PlaceCell Double -- A map from a place cell (with
                                             -- its field built only from
                                             -- spikes occurring before the
                                             -- start of the reconstruction
                                             -- timebin
                 -> Field Double             -- Pos PDF
estimatePosition = undefined