module Data.Ephys
       ( module Data.Ephys.OldMWL.FileInfo
       , module Data.Ephys.OldMWL.Parse)
       where

import Data.Ephys.EphysDefs
import qualified Data.Ephys.Spike as S
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse

-- Test serialization of TetrodeSpike
myThing = S.myTest