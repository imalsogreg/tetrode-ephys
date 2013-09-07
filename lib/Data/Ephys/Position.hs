module Position where

import Data.Ephys.EphysDefs

data Location = Location {x :: Double, y :: Double, z :: Double}
type Heading  = (Double, Double, 

data Position = Position { location :: 