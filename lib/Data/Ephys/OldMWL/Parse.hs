{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Ephys.OldMWL.Parse where

import Control.Monad (liftM, forM_, replicateM, forever)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U hiding (map, forM_, any, replicateM, zipWith)
import Data.Vector.Storable hiding (map, toList, any, replicateM, fromList, forM_, zipWith)
--import Data.Serialize
import Data.SafeCopy
import Data.Vector.Binary
import GHC.Int
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary 
import Pipes.Binary hiding (Get)
import Data.Binary.Get (runGet, runGetState)
import Pipes.ByteString (fromLazy)
import qualified Data.Text as T

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

parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | okSpikeFile fi =
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    let gains = map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs :: [Double]
    in do
      ts <- get
      wfs <- replicateM (fromIntegral hNTrodes) $ do
        liftM (U.fromList . zipWith decodeVoltage gains) (replicateM (fromIntegral hNTrodeChans) get)
      return $ MWLSpike ts wfs

dropHeader :: BSL.ByteString -> BSL.ByteString
dropHeader b = let headerEnd = "%%ENDHEADER\n"
                   rStrict = BS.drop (BS.length headerEnd) . snd . BS.breakSubstring headerEnd . BS.concat . BSL.toChunks $ b
             in BSL.fromChunks [rStrict]

produceMWLSpikes :: FileInfo -> BSL.ByteString -> Producer MWLSpike IO r
produceMWLSpikes fi b = do
  let (v,b',n) = runGetState (parseSpike fi) b 0
  yield v
  produceMWLSpikes fi b'

--spikeStream :: ByteString -> Producer TrodeSpike IO (Either )
--namedSpikeStream name fi b = decodeMany (fromLazy b) >-> PP.map snd >-> PP.map (mwlToArteSpike fi name) >-> catSpike
-- this doesn't work b/c MWLSpike can't be an instance of Binary,
-- can't be an instance of binary b/c we need FileInfo to parse,
-- there's no way to define get for an MWLSpike

catSpike :: (Monad m) => Pipe Arte.TrodeSpike Arte.TrodeSpike m r
catSpike = forever $ do
  s <- await
  yield s

mwlToArteSpike :: FileInfo -> T.Text -> MWLSpike -> Arte.TrodeSpike
mwlToArteSpike fi tName s = Arte.TrodeSpike tName tOpts tTime tWaveforms
  where tTime      = spikeTime s
        tWaveforms = spikeWaveforms s
        tOpts = 1001 -- TODO: Get trodeopts