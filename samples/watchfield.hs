module Main where

import Data.Ephys.PlaceCell
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.OldMWL.ParsePFile

import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import System.IO
import qualified Data.ByteString.Lazy as BSL
import System.Environment

-- Time now, place cells by name, occupancy map
data World = World { _now       :: Float
                   , _pos       :: TVar Position
                   , _trackPos  :: TVar (Field Double)
                   , _placeCell :: TVar PlaceCell
                   , _occupancy :: Field Double
                   } deriving (Eq)

track  = circularTrack (0,0) 0.75 0 0.2 0.25

world0     = (0, Map.empty, occupancy0)
occupancy0 = Map.fromList $ zip (allTrackPos track) [0..]
p0         = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure sZ sZ
  where sZ = take 5 (repeat 0)

streamPFile :: FilePath -> TVar World -> IO ()
streamPFile fn = do
  f <- BSL.readFile fn
  dropResult (produceMWLPos f) >-> PP.map (mwlToArtePos 

main :: IO ()
main = do
  (mwlPFile:mwlTTDirs) <- getArgs
  worldV <- newTVarIO world0
  
  print "Ok"