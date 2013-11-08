{-# LANGUAGE BangPatterns, TypeSynonymInstances, RecordWildCards, NoMonomorphismRestriction #-}

module Data.Ephys.Spike where

import Data.Ephys.TimeSignal.Filter

import Data.Text hiding (zip, map,foldl1')
import Data.Text.Encoding
import Data.List (foldl1')
import Data.Time
import Control.Monad (liftM)
import qualified Data.Serialize as S
import qualified Data.Binary as B
import qualified Data.Vector.Unboxed as U
import Data.Vector.Cereal()
import Data.Vector.Binary()
import Data.Ephys.EphysDefs

type Waveform = U.Vector Voltage  -- This should be the waveform from Data.Ephys.Waveform probably?

-- |Representation of an action potential recorded on a tetrode
data TrodeSpike = TrodeSpike { spikeTrodeName      :: !Text
                             , spikeTrodeOptsHash  :: Int
                             , spikeTime           :: ExperimentTime
                             , spikeWaveforms      :: [Waveform]
                             }
                  deriving (Show)

toRelTime :: TrodeSpike -> Double
toRelTime TrodeSpike{..} = spikeTime

instance S.Serialize TrodeSpike where
  put TrodeSpike{..} = do
    S.put (encodeUtf32LE spikeTrodeName)
    S.put spikeTrodeOptsHash
    S.put spikeTime
    S.put spikeWaveforms
  get = do
    name <- decodeUtf32LE `liftM` S.get
    opts <- S.get
    time <- S.get
    waveforms <- S.get
    return $ TrodeSpike name opts time waveforms

instance B.Binary TrodeSpike where
  put TrodeSpike{..} = do
    B.put (encodeUtf32LE spikeTrodeName)
    B.put spikeTrodeOptsHash
    B.put spikeTime
    mapM_ B.put (Prelude.map B.encode spikeWaveforms)
  get = do
    name <- decodeUtf32LE `liftM` B.get
    opts <- B.get
    time <- B.get
    waveforms <- B.get
    return $ TrodeSpike name opts time waveforms

spikePeakIndex :: TrodeSpike -> Int
spikePeakIndex s = 
  fst . foldl1' maxBySnd . map chanPeakIndexAndV . spikeWaveforms $ s

chanPeakIndexAndV :: U.Vector Voltage -> (Int,Voltage)
chanPeakIndexAndV vs = U.foldl1' maxBySnd $ U.zip (U.fromList [0..nSamps]) vs
  where nSamps = U.length vs

maxBySnd :: Ord b => (a,b) -> (a,b) -> (a,b)
maxBySnd a@(_,v) b@(_,v') = if v > v' then a else b

spikeAmplitudes  :: TrodeSpike -> [Double]
spikeAmplitudes s = map (U.! i) (spikeWaveforms s)
  where i = spikePeakIndex s

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
