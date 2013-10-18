{-# LANGUAGE BangPatterns, TypeSynonymInstances, RecordWildCards #-}

module Data.Ephys.Spike where

import Data.Ephys.TimeSignal.Filter

import Data.Text
import Data.Text.Encoding
import Data.Time
import Control.Monad (liftM)
import Control.Applicative
import Data.Traversable (traverse)
import qualified Data.ByteString.Char8 as B
import Data.Serialize
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Cereal as VS
import Data.Hashable
import Data.Ephys.EphysDefs 

type Waveform = U.Vector Voltage  -- This should be the waveform from Data.Ephys.Waveform probably?

-- |Representation of an action potential recorded on a tetrode
data TrodeSpike = TrodeSpike { spikeTrodeName      :: !Text
                             , spikeTrodeOptsHash  :: Int
                             , spikeTime           :: ExperimentTime
                             , spikeWaveforms      :: [Waveform]
                             }
                  deriving (Show)

instance Serialize TrodeSpike where
  put TrodeSpike{..} = do
    put (encodeUtf32LE spikeTrodeName)
    put spikeTrodeOptsHash
    put spikeTime
    mapM_ put (Prelude.map encode spikeWaveforms)
  get = do
    name <- decodeUtf32LE `liftM` get
    opts <- get
    time <- get
    waveforms <- get
    return $ TrodeSpike name opts time waveforms



-- |Representation of tetroe-recorded AP features
data SpikeModel = SpikeModel { mSpikeTime          :: ExperimentTime
                             , mSpikePeakAmp       :: U.Vector Voltage
                             , mSpikepPeakToTroughT :: DiffTime
                             , mSpikepPeakToTroughV :: U.Vector Voltage
                             } deriving (Show)
-- TODO: Do I need the rest of the mwl params?  maxwd? maxh?
-- What about things like 'noise'?  Or 'deviation from the cluster'?

-- |Polar coordinates representation of tetrode-recorded AP
data PolarSpikeModel = PolarSpikeModel { pSpikeTime      :: ExperimentTime
                                       , pSpikeMagnitute :: Voltage
                                       , pSpikeAngles    :: U.Vector Double
                                       } deriving (Show)


-- This should be part of arte, not tetrode-ephys?  It's about recording
-- But I need it to decode files...
data TrodeAcquisitionOpts = TrodeAcquisitionOpts { spikeFilterSpec :: FilterSpec
                                                 , spikeThresholds :: [Voltage]
                                                 } deriving (Eq, Show)


mySpike :: IO TrodeSpike
mySpike = return $ TrodeSpike tName tOpts sTime sWF
  where tName = pack "TestSpikeTrode"
        tOpts = 1001
        sTime = 10.10
        sWF = Prelude.take 4 . repeat $ (U.fromList $ [0.0 .. (31.0 :: Voltage)] :: Waveform)

myTest :: IO ()
myTest = do
  s <- mySpike
  print $ runPut (put s)
