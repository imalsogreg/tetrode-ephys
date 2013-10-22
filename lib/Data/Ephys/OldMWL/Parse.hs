{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Ephys.OldMWL.Parse where

import Control.Monad (liftM, forM_, replicateM, forever,(>=>))
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U hiding (map, forM_, any, replicateM, zipWith)
--import Data.Vector.Storable hiding (map, toList, any, replicateM, fromList, forM_, zipWith, length, head, take, drop, filter,reverse)
--import Data.Serialize
--import Data.SafeCopy
--import Data.Vector.Binary
import GHC.Int
import Foreign.C.Types
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary 
import qualified Pipes.Binary as PBinary hiding (Get)
import Data.Binary.Get (runGet, runGetState, getWord32be, getWord32le, getWord16be, getWord16le)
import qualified Pipes.ByteString as PBS 
import qualified Data.Text as T
--import Control.Monad.Trans.Either (runEitherT)
import System.Endian (fromBE32)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Packed.Matrix
import Data.Packed.Vector (Vector(..), toList)

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

decodeTime :: Word32 -> Double
decodeTime = (/ 10000) . fromIntegral

fromBE16 :: Word16 -> Word16
fromBE16 x = (x `shiftL` 8) .|. (x `shiftR` 8)

word16ToInt16 :: Word16 -> Int16
word16ToInt16 x = fromIntegral x - ( (fromIntegral (maxBound :: Word16)) `div` 2)

-- Assuming the Int16 is signed
decodeVoltage :: Double -> Word16 -> Double
decodeVoltage gain inV =
--  fromIntegral ((fromIntegral . fromBE16 $ inV) / (2 ** 14) - 10.0) * 10 / gain - 10.0
--  10.0 * ( fromIntegral inV ) / (2 ^ (14 :: Int) - 1) / gain
  fromIntegral . word16ToInt16 $ inV

spikeFromMWLSpike :: FileInfo -> MWLSpike -> Arte.TrodeSpike
spikeFromMWLSpike FileInfo{..} MWLSpike{..} = undefined

chunkToLength :: [a] -> Int -> [[a]]
chunkToLength xs n = aux [] xs
  where aux acc []  = reverse acc
        aux acc xs' = aux (take n xs' : acc) (drop n xs')

{-
getInt16be :: Get Int16
getInt16be = do
  bigByte    <- get
  littleByte <- get
  -}

hMatrixVecToUnboxedVec :: Data.Packed.Vector.Vector Double -> U.Vector Double
hMatrixVecToUnboxedVec = U.fromList . toList

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi = --FIXME
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    let allGains = map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs :: [Double]
        gains = if hProbe == 0 then take 4 allGains else take 4 . drop 4 $ allGains
        Just (_,_,totalSampsPerSpike) = listToMaybe $ filter (\(n,_,_) -> n == "waveform") hRecordDescr
    in do
      ts <- getWord32le :: Get Word32
      vs <- replicateM (fromIntegral totalSampsPerSpike) getWord16le
      let vsInt = map (fromIntegral . word16ToInt16) vs
          vsMat  = ( fromIntegral (totalSampsPerSpike `div` hNTrodeChans) >< fromIntegral hNTrodeChans ) vsInt
          vsVecs = toColumns vsMat :: [Vector Double]
          vsUVecs = map hMatrixVecToUnboxedVec vsVecs
{-      let wfs  = vs `chunkToLength` (fromIntegral totalSampsPerSpike `div` fromIntegral hNTrodeChans)
          wfsD = zipWith (\g xs -> map (decodeVoltage g) xs) gains wfs
          wfsV = map U.fromList wfsD -}
      return $ MWLSpike (decodeTime ts) vsUVecs
  | otherwise    = error "Failed okFileInfo test"

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

produceTrodeSpikes :: T.Text -> FileInfo -> BSL.ByteString -> Producer Arte.TrodeSpike IO (Either (PBinary.DecodingError, Producer BS.ByteString IO ()) ())
produceTrodeSpikes tName fi b = produceMWLSpikes fi b >-> PP.map (mwlToArteSpike fi tName)

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
  runEffect $ dropResult (produceMWLSpikes' fi f)  >-> catSpike' >-> PP.take 50 >->  PP.print
  --n <- ( PP.length $ dropResult (produceMWLSpikes' fi (dropHeaderInFirstChunk f)))
  --print n
  print "Ok"

dropResult :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()

testFile :: IO BSL.ByteString
testFile = BSL.readFile "/home/greghale/Desktop/test.tt"

testFileInfo :: IO FileInfo
testFileInfo = getFileInfo "/home/greghale/Desktop/test.tt"
