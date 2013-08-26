module Data.Ephys.OldMWL.Parse where

import Data.ByteString
import Data.Vector
import Data.Vector.Storable
import Data.Serialize
import Data.SafeCopy
import Data.Vector.Binary
import Data.Ephys.OldMWL.FileInfo

data MWLSpike = MWLSpike { spikeTime      :: Double
                         , spikeWaveforms :: [Vector Double]
                         } deriving (Eq, Show)

okSpikeFile :: FileInfo -> Bool
okSpikeFile FileInfo{..} = 

parseSpike :: FileInfo -> ByteString -> Get MWLSpike
parseSpike FileInfo{..} b
  | hRecMode == Spike =
    let 
  