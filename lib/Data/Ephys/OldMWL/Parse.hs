{-# LANGUAGE RecordWildCards #-}

module Data.Ephys.OldMWL.Parse where

import Control.Monad (liftM, forM_, replicateM)
import Data.ByteString hiding (map, any, zipWith)
import Data.Vector hiding (map, forM_, any, replicateM, zipWith)
import Data.Vector.Storable hiding (map, toList, any, replicateM, fromList, forM_, zipWith)
import Data.Serialize
import Data.SafeCopy
import Data.Vector.Binary
import GHC.Int

import Data.Ephys.Spike
import Data.Ephys.OldMWL.FileInfo

data MWLSpike = MWLSpike { spikeTime      :: Double
                         , spikeWaveforms :: [Data.Vector.Vector Double]
                         } deriving (Eq, Show)

okSpikeFile :: FileInfo -> Bool
okSpikeFile FileInfo{..} = hRecMode == Spike
                           && any (\(name,_,_) -> name == "timestamp") hRecordDescr -- has "timestamp"
                           && any (\(name,_,_) -> name == "waveform")  hRecordDescr -- has "waveform"
                           && Prelude.filter (\(name,_,_) -> name == "timestamp") hRecordDescr
                           == [("timestamp", DULong, 1)]    -- timestamp's time is DULong
                           && (\[(_,tp,_)] -> tp == DShort)
                           (Prelude.filter (\(name,_,_) -> name == "waveform") hRecordDescr)
                           -- waveform elem's type is DShort (16-bit)

writeSpike :: MWLSpike -> Put
writeSpike (MWLSpike tSpike waveforms) = do put tSpike
                                            forM_ waveforms $ \waveform ->
                                              forM_ (toList waveform) put

decodeTime :: Int32 -> Double
decodeTime = fromIntegral . (`div` 10000)

decodeVoltage :: Double -> Int16 -> Double
decodeVoltage gain inV =
  fromIntegral (inV `div` 2^(14 :: Int16)) * 10 / gain

spikeFromMWLSpike :: FileInfo -> MWLSpike -> Spike
spikeFromMWLSpike FileInfe{..} MWLSpike{..} =

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi =
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    let gains = map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs :: [Double]
    in do
      ts <- get
      wfs <- replicateM (fromIntegral hNTrodes) $ do
        liftM (fromList . zipWith decodeVoltage gains) (replicateM (fromIntegral hNTrodeChans) get)
      return $ MWLSpike ts wfs

spikeStream :: Producer TrodeSpike 