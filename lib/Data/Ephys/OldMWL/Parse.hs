{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Ephys.OldMWL.Parse where

import Control.Monad (liftM, forM_, replicateM, forever,(>=>))
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U hiding (map, forM_, any, replicateM, zipWith)
import Data.Vector.Storable hiding (map, toList, any, replicateM, fromList, forM_, zipWith, length, head, take, drop, filter,reverse)
--import Data.Serialize
--import Data.SafeCopy
import Data.Vector.Binary
import GHC.Int
import Foreign.C.Types
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary 
import qualified Pipes.Binary as PBinary hiding (Get)
import Data.Binary.Get (runGet, runGetState)
import qualified Pipes.ByteString as PBS 
import qualified Data.Text as T
import Control.Monad.Trans.Either (runEitherT)
import System.Endian (fromBE32)

import qualified Data.Ephys.Spike as Arte
import Data.Ephys.OldMWL.FileInfo

data MWLSpike = MWLSpike { spikeTime      :: Double
                         , spikeWaveforms :: [U.Vector Double]
                         } deriving (Eq, Show)

okSpikeFile :: FileInfo -> Bool
okSpikeFile FileInfo{..} = hRecMode == Spike
                           && hRecordDescr `hasField` "timestamp"
                           && hRecordDescr `hasField` "waveform"
                           && fieldIsType hRecordDescr "timestamp" DULong
                           && fieldIsType hRecordDescr "waveform"  DShort
                           -- waveform elem's type is DShort (16-bit)

okPosFile :: FileInfo -> Bool
okPosFile FileInfo{..} = hRecMode == Tracker
                         && hasField hRecordDescr "timestamp"
                         && hasField hRecordDescr "xfront"
                         && hasField hRecordDescr "yfront"
                         && hasField hRecordDescr "xback"
                         && hasField hRecordDescr "yback"

hasField :: [RecordDescr] -> String -> Bool
hasField flds f = any (\(name,_,_) -> name == f) flds

fieldIsType :: [RecordDescr] -> String -> DatumType -> Bool
fieldIsType flds f t = Prelude.all (==True) [ (fn == f) <= (ft ==t) | (fn,ft,_) <- flds ]

writeSpike :: MWLSpike -> Put
writeSpike (MWLSpike tSpike waveforms) = do put tSpike
                                            forM_ waveforms $ \waveform ->
                                              forM_ (U.toList waveform) put

decodeTime :: Int32 -> Double
decodeTime = fromIntegral . (`div` 10000)

decodeVoltage :: Double -> Int16 -> Double
decodeVoltage gain inV =
  fromIntegral (inV `div` 2^(14 :: Int16)) * 10 / gain

spikeFromMWLSpike :: FileInfo -> MWLSpike -> Arte.TrodeSpike
spikeFromMWLSpike FileInfo{..} MWLSpike{..} = undefined

chunkToLength :: [a] -> Int -> [[a]]
chunkToLength xs n = aux [] xs
  where aux acc []  = reverse acc
        aux acc xs' = aux (take n xs' : acc) (drop n xs')

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi = --FIXME
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    let allGains = map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs :: [Double]
        gains = if hProbe == 0 then take 4 allGains else take 4 . drop 4 $ allGains
        Just (_,_,totalSampsPerSpike) = listToMaybe $ filter (\(n,_,_) -> n == "waveform") hRecordDescr
    in do
      ts <- get
      vs <- replicateM (fromIntegral totalSampsPerSpike) get :: Get [Int16]
      let wfs  = vs `chunkToLength` (fromIntegral totalSampsPerSpike `div` fromIntegral hNTrodeChans)
          wfsD = zipWith (\g xs -> map (decodeVoltage g) xs) gains wfs
          wfsV = map U.fromList wfsD
      return $ MWLSpike ts wfsV
  | otherwise    = error "Failed okFileInfo test"

testParse' :: IO ()
testParse' = do
  f <- testFile
  fi <- testFileInfo
  let s = runGet (parseSpike fi) f
  print s

testParse :: Get (Word32,[Int16])
testParse = do
  ts <- get
  vs <- replicateM 128 get
  return (fromBE32 ts,vs)

myTest'' :: IO ()
myTest'' = do
  f <- testFile
  let xs = runGet (replicateM 400 testParse) (dropHeaderInFirstChunk f)
  Prelude.mapM_ (\(t,vs) -> print t >> print (vs `chunkToLength` 32)) xs

dropHeaderInFirstChunk :: BSL.ByteString -> BSL.ByteString
dropHeaderInFirstChunk b = let headerEnd = "%%ENDHEADER\n"
                               firstChunk = head . BSL.toChunks $ b
                               (h,t) = BS.breakSubstring headerEnd firstChunk
                           in
                            if BS.null t
                            then b
                            else BSL.drop (fromIntegral (BS.length h + BS.length headerEnd)) b

produceMWLSpikes :: FileInfo -> BSL.ByteString -> Producer MWLSpike IO r
produceMWLSpikes fi b = aux (dropHeaderInFirstChunk b)
  where
    aux headerlessB = do
      let (v,b',n) = runGetState (parseSpike fi) headerlessB 0
      lift $ putStrLn "Test"
      yield v
      lift $ putStrLn "Test2"
      aux b'


produceMWLSpikes' :: FileInfo -> BSL.ByteString -> Producer MWLSpike IO (Either (PBinary.DecodingError, Producer BS.ByteString IO ()) ())
produceMWLSpikes' fi b = let myGet = parseSpike fi in
  PBinary.decodeGetMany myGet (PBS.fromLazy . dropHeaderInFirstChunk $ b) >-> PP.map snd

--spikeStream :: ByteString -> Producer TrodeSpike IO (Either )
--namedSpikeStream name fi b = decodeMany (fromLazy b) >-> PP.map snd >-> PP.map (mwlToArteSpike fi name) >-> catSpike
-- this doesn't work b/c MWLSpike can't be an instance of Binary,
-- can't be an instance of binary b/c we need FileInfo to parse,
-- there's no way to define get for an MWLSpike

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
  where tTime      = spikeTime s
        tWaveforms = spikeWaveforms s
        tOpts = 1001 -- TODO: Get trodeopts

myTest :: IO ()
myTest = do
  f <- BSL.readFile "/home/greghale/Desktop/test.tt"
  fi <- getFileInfo "/home/greghale/Desktop/test.tt"
  runEffect $ produceMWLSpikes' fi f  >-> catSpike' >->  PP.print
  
  print "Ok"

dropResult :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()

testFile :: IO BSL.ByteString
testFile = BSL.readFile "/home/greghale/Desktop/test.tt"

testFileInfo :: IO FileInfo
testFileInfo = getFileInfo "/home/greghale/Desktop/test.tt"

myTest' :: IO ()
myTest' = do
  f <- BSL.readFile "/home/greghale/Desktop/test.tt"
  fi <- getFileInfo "/home/greghale/Desktop/test.tt"
--  runEffect $ toUnit (
  print "Ok"