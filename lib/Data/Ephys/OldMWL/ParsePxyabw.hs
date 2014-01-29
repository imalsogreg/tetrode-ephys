{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Ephys.OldMWL.ParsePxyabw where

import qualified Data.Ephys.Spike as Arte
import Data.Ephys.OldMWL.Parse -- We're using the old file organization.  b/c hasty
import Data.Ephys.OldMWL.FileInfo

import Control.Monad (forM_, replicateM, forever)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U
import GHC.Int
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary
import Data.Binary.Put
import qualified Pipes.Binary as PBinary hiding (Get)
import Data.Binary.Get (getWord32le, getWord16le,getWord64le, getWord64be)
import qualified Pipes.ByteString as PBS 
import qualified Data.Text as T
import Data.Packed.Matrix
import qualified Data.List as List
import Data.Packed.Vector (Vector, toList)
import Control.Applicative
import Data.Binary.IEEE754

data MWLSpikeParms = MWLSpikeParms { mwlSParmsID    :: Word32
                                   , mwlSParmsTpX   :: Word16
                                   , mwlSParmsTpY   :: Word16
                                   , mwlSParmsTpA   :: Word16
                                   , mwlSParmsTpB   :: Word16
                                   , mwlSParmsTMaxW :: Word16
                                   , mwlSParmsTMaxH :: Word16
                                   , mwlSParmsTime  :: Double
                                   , mwlSParmsPosX  :: Word16
                                   , mwlSParmsPosY  :: Word16
                                   , mwlSParmsVel   :: Word32
                                   } deriving (Eq, Show)


parsePxyabw :: Get MWLSpikeParms
parsePxyabw = MWLSpikeParms
              <$> getWord32le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> getWord16le
              <*> (wordToDouble <$> getWord64le)
              <*> getWord16le
              <*> getWord16le
              <*> getWord32le
              
writeSpikeParms :: MWLSpikeParms -> Put
writeSpikeParms MWLSpikeParms{..} = do
  putWord32le $ mwlSParmsID
  putWord16le $ mwlSParmsTpX
  putWord16le $ mwlSParmsTpY
  putWord16le $ mwlSParmsTpA
  putWord16le $ mwlSParmsTpB
  putWord16le $ mwlSParmsTMaxW
  putWord16le $ mwlSParmsTMaxH
  putWord64le $ doubleToWord mwlSParmsTime
  putWord16le $ mwlSParmsPosX
  putWord16le $ mwlSParmsPosY
  putWord32le    $ mwlSParmsVel
  