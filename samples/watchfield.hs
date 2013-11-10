{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Ephys.PlaceCell
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.OldMWL.ParsePFile

import Control.Monad
import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import System.IO
import qualified Data.ByteString.Lazy as BSL
import System.Environment
import Pipes
import Control.Lens
import Pipes.RealTime

-- Time now, place cells by name, occupancy map
data World = World { _now       :: Float
                   , _pos       :: TVar Position
                   , _trackPos  :: TVar (Field Double)
                   , _placeCell :: TVar PlaceCell
                   , _occupancy :: TVar (Field Double)
                   } deriving (Eq)
$(makeLenses ''World)

track  = circularTrack (0,0) 0.75 0 0.2 0.25

world0 :: PlaceCell -> IO World
world0 placeCell0 = do
  pos0  <- newTVarIO p0
  tPos0 <- newTVarIO $ posToField track p0 kern
  cell0 <- newTVarIO $ placeCell0
  occ0  <- newTVarIO $ Map.empty
  return $ World 0 pos0 tPos0 cell0 occ0
  

kern = PosGaussian 0.2
occupancy0 = Map.fromList $ zip (allTrackPos track) [0..]
p0         = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure sZ sZ
  where sZ = take 5 (repeat 0)

streamPFile :: FilePath -> TVar World -> Double -> 
               (Double,Double) -> Double -> Double -> IO ()
streamPFile fn world fileT0 (pX0,pY0) pixPerM h = do
  f <- BSL.readFile fn
  runEffect $
    dropResult (produceMWLPos f) >-> 
    runningPosition (pX0,pY0) pixPerM h p0 >->
    relativeTimeCatDelayedBy _posTime fileT0 >->
    (forever $ do
        pos' <- await
        lift . atomically $ do 
          w   <- readTVar world
          occ <- readTVar (w^.occupancy)
          let posField = posToField track pos' kern
          writeTVar (w^.pos) pos'
          writeTVar (w^.trackPos)  posField
          writeTVar (w^.occupancy) (updateField (+) occ posField))
    
          


main :: IO ()
main = do
  [mwlPFile,mwlTTDir] <- getArgs
  worldV <- newTVarIO world0
  
  print "Ok"
  
dropResult ::  (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()