{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Ephys.OldMWL.Parse where

import Control.Monad (forM_, replicateM, forever)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed hiding (map, forM_, any, replicateM,
                                   zipWith, drop, take, head,
                                   filter, (++), all, toList)
import GHC.Int
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary
import Data.Binary.Put
import qualified Pipes.Binary as PBinary hiding (Get)
import Data.Binary.Get (getWord32le, getWord16le)
import qualified Pipes.ByteString as PBS 
import qualified Data.Text as T
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Packed.Matrix
import qualified Data.List as List
import Data.Packed.Vector (Vector, toList)

import qualified Data.Ephys.Spike as Arte
import Data.Ephys.OldMWL.FileInfo

data MWLSpike = MWLSpike { mwlSpikeTime      :: Double
                         , mwlSpikeWaveforms :: [U.Vector Double]
                         } deriving (Eq, Show)


writeSpike :: MWLSpike -> Put
writeSpike (MWLSpike tSpike waveforms) = do
  putWord32le $ encodeTime tSpike
  let vs = List.concat . List.transpose . map U.toList $ waveforms
  forM_ vs $ 
    putWord16le . int16toWord16 . voltageToMwlUnits
{-
forM_ waveforms $ \waveform ->
                                              forM_ (U.toList waveform) put
-}
decodeTime :: Word32 -> Double
decodeTime = (/ 10000) . fromIntegral

encodeTime :: Double -> Word32
encodeTime = floor . (* 10000)

{- Unused!
fromBE16 :: Word16 -> Word16
fromBE16 x = (x `shiftL` 8) .|. (x `shiftR` 8)
-}

-- 'cast'int word to int, right?
word16ToInt16 :: Word16 -> Int16
word16ToInt16 x = fromIntegral x - ( (fromIntegral (maxBound :: Word16)) `div` 2)

-- TODO Is this right?
int16toWord16 :: Int16 -> Word16
int16toWord16 x = fromIntegral x

-- MWL units go as -2^13 -> (2^13-1)  => -10V -> 10V
mwlUnitsToVoltage :: Double -> Double -> Double
mwlUnitsToVoltage gain inV = inV * cVG/bMWL + (zMWL / bMWL)
  where bMWL= 2^(13::Int) - 1
        zMWL = -1/2^(15::Int)
        cVG  = 10 / gain

voltageToMwlUnits :: Double -> Double -> Double
voltageToMwlUnits gain inV = inV / (mwlUnitsToVoltage gain 1.0)
    
-- Assuming the Int16 is signed
wordToMWLDouble :: Word16 -> Double
wordToMWLDouble inV =
  fromIntegral . word16ToInt16 $ inV

chunkToLength :: [a] -> Int -> [[a]]
chunkToLength xs n = aux [] xs
  where aux acc []  = Prelude.reverse acc
        aux acc xs' = aux (Prelude.take n xs' : acc) (Prelude.drop n xs')

hMatrixVecToUnboxedVec :: Data.Packed.Vector.Vector Double -> U.Vector Double
hMatrixVecToUnboxedVec = U.fromList . Data.Packed.Vector.toList

fileGains :: FileInfo -> [Double]
fileGains FileInfo{..} = let gains' = map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs in
  case hProbe of
    0 -> take 4 gains'
    1 -> take 4 . drop 4 $ gains'
    n -> error $ "Can't have probe " ++ show n

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi = --FIXME
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    let gains = fileGains fi
        Just (_,_,totalSampsPerSpike) = listToMaybe $ filter (\(n,_,_) -> n == "waveform") hRecordDescr
    in do
      ts <- getWord32le :: Get Word32
      -- grab one sample at a time (word16) from the stream: nchans * nsampsperchan
      vs <- replicateM (fromIntegral totalSampsPerSpike) getWord16le
      let fI = fromIntegral
          vsInt = map (fI . word16ToInt16) vs
          -- Make a matrix of the sample vector (>< is from Data.Packed.Matrix)
          vsMat  = fI (totalSampsPerSpike `div` hNTrodeChans) >< fI hNTrodeChans $ vsInt
          -- Transpose it into a list (toColumns from Data.Packed.Matrix)
          vsVecs = toColumns vsMat :: [Data.Packed.Vector.Vector Double]
          -- From Matrix's Vector to the usual Vector
          vsUVecs = map hMatrixVecToUnboxedVec vsVecs
      return $ MWLSpike (decodeTime ts) vsUVecs
  | otherwise    = error "Failed okFileInfo test"

produceMWLSpikes :: FileInfo -> BSL.ByteString -> Producer MWLSpike IO (Either (PBinary.DecodingError, Producer BS.ByteString IO ()) ())
produceMWLSpikes fi b = let myGet = parseSpike fi in
  PBinary.decodeGetMany myGet (PBS.fromLazy . dropHeaderInFirstChunk $ b) >-> PP.map snd

produceTrodeSpikes :: T.Text -> FileInfo -> BSL.ByteString -> Producer Arte.TrodeSpike IO (Either (PBinary.DecodingError, Producer BS.ByteString IO ()) ())
produceTrodeSpikes tName fi b = produceMWLSpikes fi b >-> PP.map (mwlToArteSpike fi tName)

-- TODO: This is misplaced.  Need something like "general utils."
dropResult :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()

catSpike :: (Monad m) => Pipe Arte.TrodeSpike Arte.TrodeSpike m r
catSpike = forever $ do
  s <- await
  yield s

catSpike' :: Pipe MWLSpike MWLSpike IO r
catSpike' = forever $ do
  s <- await
  lift $ putStrLn "catSpike'"
  yield s

mwlToArteSpike :: FileInfo -> T.Text -> MWLSpike -> Arte.TrodeSpike
mwlToArteSpike fi tName s = Arte.TrodeSpike tName tOpts tTime tWaveforms
  where tTime      = mwlSpikeTime s
        gains      = fileGains fi
        tWaveforms = Prelude.zipWith
                     (\g -> U.map (mwlUnitsToVoltage g)) gains (mwlSpikeWaveforms s)
        tOpts = 1001 -- TODO: Get trodeopts
