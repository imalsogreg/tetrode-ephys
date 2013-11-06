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

produceMWLPos :: Double -> Double -> Double -> BSL.ByteString ->
                 Producer MWLPos IO (Either (PBinary.DecodingError,
                                             Producer BS.ByteString IO ()) ())
produceMWLPos pX0 pY0 pixPerMeter f = 
  PBinary.decodeGetMany parsePRecord (PBS.fromLazy . dropHeaderInFirstChunk $ f) >-> PP.map snd

parsePRecord :: Double -> Double -> Double -> Binary.Get MWLPos
parsePRecord pX0 pY0 pixPerMeter =
  let s = 1/pixPerMeter
      pXToArte = (*s) . (subtract pX0)
      pYToArte = (*s) . (subtract pY0)
  in  do
    recTs    <- getWord32le
    recXf    <- getWord16le
    recYf    <- getWord16le
    recXb    <- getWord16le
    recYb    <- getWord16le
    return $ MWLPos (decodeTime recTs)
      (fI recXf)
      (fI recYf)
      (fI recXb)
      (fI recYb)

mwlToArtePos :: (Double,Double)
             -> Double
             -> Double
             -> MWLPos
             -> Position
             -> Position
mwlToArtePos (pX0,pY0) pixelsPerMeter height m p =
  let s = 1/pixelsPerMeter
      pXToArte = (*s) . (subtract pX0)
      pYToArte = (*s) . (subtract pY0)
      fX = fI $ m^.mwlPxf :: Double
      fY = fI $ m^.mwlPyf :: Double
      bX = fI $ m^.mwlPxb :: Double
      bY = fI $ m^.mwlPyb :: Double
      x = pXToArte $ avg2 fX bX
      y = pXToArte $ avg2 fY bY
      loc = Location x y height
      angle = Angle (atan2 (fY - bY) (fX - bX)) 0 0
      conf = if all (> 0) [fX,bX,fY,bY] then ConfSure else ConfUnsure
  in
  stepPos p (m^.mwlPosTime) loc angle conf


avg2 :: Double -> Double -> Double
avg2 a b = (a+b)/2


fI :: (Num a, Integral b) => b -> a
fI = fromIntegral
