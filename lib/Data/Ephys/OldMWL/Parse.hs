{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Ephys.OldMWL.Parse where

import Data.Ephys.OldMWL.Header
import Control.Monad (forM_, replicateM, forever)
--import Control.Monad.Trans.State
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Int
import Control.Lens (view)
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary
import Data.Binary.Put
import qualified Pipes.Binary as PBinary hiding (Get)
import Pipes.Binary (decoded, decodeGetL,decodeGet)
import Data.Binary.Get (getWord32le, getWord16le)
import qualified Pipes.ByteString as PBS 
import qualified Data.Text as T
import Data.Packed.Matrix
import qualified Data.List as List
import Data.Packed.Vector (Vector, toList)

import Pipes.Parse

import qualified Data.Ephys.Spike as Arte
import Data.Ephys.OldMWL.FileInfo

data MWLSpike = MWLSpike { mwlSpikeTime      :: Double
                         , mwlSpikeWaveforms :: [U.Vector Double] -- Double is
                                                                  -- MWL units
                                                                  -- though
                         } deriving (Eq, Show)


writeSpike :: FileInfo -> MWLSpike -> Put
writeSpike fi (MWLSpike tSpike waveforms) = do
  putWord32le $ encodeTime tSpike
  let vs = List.concat . List.transpose . map U.toList $ waveforms
  forM_ vs $ 
    putWord16le . int16toWord16 . floor 

decodeTime :: Word32 -> Double
decodeTime = (/ 10000) . fromIntegral

encodeTime :: Double -> Word32
encodeTime = floor . (* 10000)


-- 'cast'int word to int, right?
word16ToInt16 :: Word16 -> Int16
word16ToInt16 x = fromIntegral x - ( (fromIntegral (maxBound :: Word16)) `div` 2) - 1

word32ToInt32 :: Word32 -> Int32
word32ToInt32 x = fromIntegral x - ( (fromIntegral (maxBound :: Word32)) `div` 2) - 1

-- TODO Is this right?
int16toWord16 :: Int16 -> Word16
int16toWord16 x = fromIntegral x - fromIntegral (maxBound :: Word16) - 1

int32toWord32 :: Int32 -> Word32
int32toWord32 x = fromIntegral x - fromIntegral (maxBound :: Word32) - 1


{-
-- Test this out. Didn't work!  Turned good import bad.
word16ToInt16 = fromIntegral
word32ToInt32 = fromIntegral
int16toWord16 = fromIntegral
int32toWord32 = fromIntegral
-}

-- MWL units go as -2^13 -> (2^13-1)  => -10V -> 10V
mwlUnitsToVoltage :: Double -> Double -> Double
mwlUnitsToVoltage gain inV = inV * cVG/bMWL + (zMWL / bMWL)
  where bMWL= 2^(13::Int) - 1
        zMWL = -1/2^(15::Int)
        cVG  = 10 / gain

voltageToMwlUnits :: Double -> Double -> Double
voltageToMwlUnits gain inV = inV / (mwlUnitsToVoltage gain 1.0)
    

chunkToLength :: [a] -> Int -> [[a]]
chunkToLength xs n = aux [] xs
  where aux acc []  = Prelude.reverse acc
        aux acc xs' = aux (Prelude.take n xs' : acc) (Prelude.drop n xs')

hMatrixVecToUnboxedVec :: Data.Packed.Vector.Vector Double -> U.Vector Double
hMatrixVecToUnboxedVec = U.fromList . Data.Packed.Vector.toList

fileGains :: FileInfo -> [Double]
fileGains FileInfo{..} =
  let gains' =
        map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs in
  case hProbe of
    0 -> take 4 gains'
    1 -> take 4 . drop 4 $ gains'
    n -> error $ "Can't have probe " ++ show n

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi = --FIXME
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    --let gains = fileGains fi
    -- TODO REMOVE PARTIAL FUNCTION
    let Just (_,_,totalSampsPerSpike) = listToMaybe $ filter (\(n,_,_) -> n == "waveform") hRecordDescr
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


------------------------------------------------------------------------------
produceMWLSpikes :: FileInfo -> BSL.ByteString -> Producer MWLSpike IO ()
produceMWLSpikes fi b =
  let myGet = parseSpike fi :: Get MWLSpike
      bytes = PBS.fromLazy . dropHeaderInFirstChunk $ b
  in dropResult (getMany myGet bytes)

getMany :: Monad m => Get a -> Producer PBS.ByteString m r -> Producer a m PBinary.DecodingError
getMany getA = go
  where go p = do
          (x, p') <- lift $ runStateT (decodeGet getA) p
          case x of
            Left err -> return err
            Right a -> do
              yield a
              go p'

produceMWLSpikesFromFile :: FilePath -> Producer MWLSpike IO ()
produceMWLSpikesFromFile fn = do
  fi' <- liftIO $ getFileInfo fn
  r   <- liftIO $ loadRawMWL fn
  case (r,fi') of
--    Left e -> do
--      liftIO $ putStrLn $ "Couldn't open filename" ++ fn
    (Right (_,dataBits), Right fi) ->
      dropResult $ produceMWLSpikes fi dataBits

--produceTrodeSpikes :: Int -> FileInfo -> BSL.ByteString -> Producer Arte.TrodeSpike IO (Either (PBinary.DecodingError, Producer BS.ByteString IO ()) ())
produceTrodeSpikes :: Int -> FileInfo -> BSL.ByteString -> Producer Arte.TrodeSpike IO ()
produceTrodeSpikes tName fi b = produceMWLSpikes fi b >-> PP.map (mwlToArteSpike fi tName)

-- TODO Int to TrodeName
produceTrodeSpikesFromFile :: FilePath -> Int -> Producer Arte.TrodeSpike IO ()
produceTrodeSpikesFromFile fn trodeName = do
  fi' <- liftIO . getFileInfo $ fn
  r'  <- liftIO . loadRawMWL  $ fn
  case (fi',r') of
    (Right fi, Right (_,dataBytes)) ->
     dropResult $ produceTrodeSpikes trodeName fi dataBytes
    (Left e1, Left e2) ->
      error $ fn ++ ": Bad fileinfo and file: " ++ e1 ++ " " ++ e2
    (Left e1,_) -> error $ fn ++ ": Bad fileinfo: " ++ e1
    (_,Left e2) -> error $ fn ++ ": Bad file: " ++ e2   

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

mwlToArteSpike :: FileInfo -> Int -> MWLSpike -> Arte.TrodeSpike
mwlToArteSpike fi tName s = Arte.TrodeSpike tName tOpts tTime tWaveforms
  where tTime      = mwlSpikeTime s
        gains      = V.fromList $ fileGains fi
        tWaveforms = V.zipWith
                     (\g -> U.map (mwlUnitsToVoltage g))
                     gains
                     (V.fromList $ mwlSpikeWaveforms s)
        tOpts = 1001 -- TODO: Get trodeopts

-- "path/to/0224.tt" -> "24"
-- TODO : Fix.  Only drop 5 when extention has 2 letters.
mwlTrodeNameFromPath :: String -> T.Text
mwlTrodeNameFromPath = T.pack . reverse . take 2 . drop 5 . reverse  



