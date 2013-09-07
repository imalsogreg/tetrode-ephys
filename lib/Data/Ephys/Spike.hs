{-# LANGUAGE BangPatterns, TypeSynonymInstances, RecordWildCards #-}

module Data.Ephys.Spike where

import Data.Ephys.TimeSignal.Filter

import Data.Text
import Data.Time
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Data.SafeCopy
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Serialize as VS
import Data.Ephys.EphysDefs 

type Waveform = U.Vector Voltage  -- This should be the waveform from Data.Ephys.Waveform probably?

-- |Representation of an action potential recorded on a tetrode
data TrodeSpike = TrodeSpike { spikeTrodeName      :: !Text
                             , spikeTrodeOptsHash  :: !Text   -- hash hashes values to Text?
                             , spikeTime           :: ExperimentTime
                             , spikeWaveforms      :: [Waveform]
                             }
                  deriving (Show)
                  --deriving (Show, Typeable, Data, Generics.Deriving.Generic)

instance SafeCopy TrodeSpike where
  putCopy TrodeSpike{..} = contain $ do
    safePut spikeTrodeName
    safePut spikeTime
    VS.genericPutVector spikeWaveforms
  getCopy = contain $ TrodeSpike <$> safeGet <*> safeGet <*> safeGet <*> VS.genericGetVector



-- |Representation of tetroe-recorded AP features
data SpikeModel = SpikeModel { mSpikeTime          :: ExperimentTime
                             , mSpikePeakAmp       :: V.Vector Voltage
                             , mSpikepPeakToTroughT :: DiffTime
                             , mSpikepPeakToTroughV :: V.Vector Voltage
                             } deriving (Show)
-- TODO: Do I need the rest of the mwl params?  maxwd? maxh?
-- What about things like 'noise'?  Or 'deviation from the cluster'?

-- |Polar coordinates representation of tetrode-recorded AP
data PolarSpikeModel = PolarSpikeModel { pSpikeTime      :: ExperimentTime
                                       , pSpikeMagnitute :: Voltage
                                       , pSpikeAngles    :: V.Vector Double
                                       } deriving (Show)


-- This should be part of arte, not tetrode-ephys?  It's about recording
-- But I need it to decode files...
data TrodeAcquisitionOpts = TrodeAcquisitionOpts { spikeFilterSpec :: FilterSpec
                                                 , spikeThresholds :: [Voltage]
                                                 } deriving (Eq, Show)


mySpike :: IO TrodeSpike
mySpike = return $ TrodeSpike tName tOpts sTime sWF
  where tName = pack "TestSpikeTrode"
        tOpts = pack "noOpts"
        sTime = 10.10
        sWF = Prelude.take 4 . repeat $ (V.fromList $ [0.0 .. (31.0 :: Voltage)] :: Waveform)

myTest :: IO ()
myTest = do
  s <- mySpike
  print $ runPut (safePut s)
