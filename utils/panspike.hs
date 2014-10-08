{-# LANGUAGE GADTs #-}

module Panspike (
  readMWLSpike,
  readMWLPos,
--  readMWLEEG,
  readTrodeSpike,
  readEeg,
  readPos,
  readTrackPos,

  writeMWLSpike,
  writeMWLPos,
  writeTrodeSpike,
  writeEeg,
  writePos,
  writeTrackPos
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString      as BS

data Target = TargetMWLSpike
            | TargetMWLPos  
            | TargetTrackPos
            | TargetSpike   
            -- | TargetEEG
            -- | TargetMWLEEG

data Format = JsonRaw
            | BinaryRaw
            | JsonMessage
            | BinaryMessage

------------------------------------------------------------------------------
convert :: (Format, Target) -> (Format, Target) -> BSL.ByteString
           -> BSL.ByteString
convert (inFmt, inTy) (outFmt, outTy) inBs =
  case (inFmt, outFmt) of
    (TargetMWLSpike)