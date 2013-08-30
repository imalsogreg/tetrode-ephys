{-# LANGUAGE RecordWildCards #-}

module Data.Ephys.OldMWL.Parse where

import Control.Monad (liftM, forM_, replicateM)
import Data.ByteString hiding (map, any)
import Data.Vector hiding (map, forM_, any, replicateM)
import Data.Vector.Storable hiding (map, toList, any, replicateM, fromList, forM_)
import Data.Serialize
import Data.SafeCopy
import Data.Vector.Binary
import GHC.Int

import Data.Ephys.OldMWL.FileInfo

data MWLSpike = MWLSpike { spikeTime      :: Double
                         , spikeWaveforms :: [Data.Vector.Vector Double]
                         } deriving (Eq, Show)

okSpikeFile :: FileInfo -> Bool
okSpikeFile FileInfo{..} = hRecMode == Spike
                           && any (\(name,_,_) -> name == "timestamp") hRecordDescr
                           && any (\(name,_,_) -> name == "waveform")  hRecordDescr

writeSpike :: MWLSpike -> Put
writeSpike (MWLSpike tSpike waveforms) = do put tSpike
                                            forM_ waveforms $ \waveform ->
                                              forM_ (toList waveform) put

decodeVoltage :: Double -> Int16 -> Double
decodeVoltage gain inV =
  fromIntegral (inV `div` 2^(14 :: Int16)) * 10 / gain

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi =
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    let (Just (_,tsType,1))       = lookup "timestamp" (map (\(n,a,b) -> (n,(n,a,b))) hRecordDescr)
        -- wfType unused b/c we're assuming it's int16.  NB - only the first 14 bits are used to fill
        -- the recording range!
        (Just (_,wfType,wfCount)) = lookup "waveform"  (map (\(n,a,b) -> (n,(n,a,b))) hRecordDescr)
        nChans                    = hNTrodeChans
        nSampsPerChan = wfCount `div` nChans
    in do
      ts <- get
      wfs <- replicateM nChans $ do
        liftM fromList (replicateM nSampsPerChan get)
      return $ MWLSpike ts wfs