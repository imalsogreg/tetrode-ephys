module Main where

import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.Position
import Data.Ephys.TrackPosition

import Graphics.Gloss

fI = fromIntegral

trackPosPicture :: TrackPos -> Picture
trackPosPicture (TrackPos bin dir ecc) = case bin of
  TrackBin _ (Location x y _) dir start stop
  Polygon [( ),( ),( ),( )]