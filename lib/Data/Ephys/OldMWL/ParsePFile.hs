{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.OldMWL.ParsePFile where

import Data.Ephys.Position
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse (decodeTime, dropResult)

import Control.Lens
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Pipes.ByteString as PBS
import qualified Pipes.Binary as PBinary
import GHC.Int
import Pipes
import qualified Pipes.Prelude as PP
import qualified Data.Binary as Binary
import Data.Binary.Get (getWord32le, getWord16le)



data MWLPos = MWLPos { _mwlPosTime  :: Double
                     , _mwlPxf      :: Int
                     , _mwlPyf      :: Int
                     , _mwlPxb      :: Int
                     , _mwlPyb      :: Int
                     } deriving (Eq, Show)

$(makeLenses ''MWLPos)

produceMWLPos :: BSL.ByteString ->
                 Producer MWLPos IO (Either (PBinary.DecodingError,
                                             Producer BS.ByteString IO ()) ())
produceMWLPos f = 
  PBinary.decodeGetMany parsePRecord (PBS.fromLazy . dropHeaderInFirstChunk $ f) >-> PP.map snd

parsePRecord :: Binary.Get MWLPos
parsePRecord = do
  recTs    <- getWord32le
  recXf    <- getWord16le
  recYf    <- getWord16le
  recXb    <- getWord16le
  recYb    <- getWord16le
  return $ MWLPos (decodeTime recTs)
    (fromIntegral recXf) (fromIntegral recYf) (fromIntegral recXb) (fromIntegral recYb)