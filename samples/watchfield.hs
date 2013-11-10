{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import Data.Ephys.Spike
import Data.Ephys.PlaceCell
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.Cluster
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.ParseClusterFile

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
data World = World { _now        :: Float
                   , _pos        :: TVar Position
                   , _trackPos   :: TVar (Field Double)
                   , _placeCells :: [TVar PlaceCell]
                   , _occupancy  :: TVar (Field Double)
                   } deriving (Eq)
$(makeLenses ''World)

track  = circularTrack (0,0) 0.75 0 0.2 0.25

world0 :: Map.Map Int ClusterMethod -> IO World
world0 clusters = do
  pos0  <- newTVarIO p0
  tPos0 <- newTVarIO $ posToField track p0 kern
  cells <- forM (Map.elems clusters) (\c -> newTVarIO (PlaceCell c Map.empty))
  occ0  <- newTVarIO $ Map.empty
  return $ World 0 pos0 tPos0 cells occ0
  

kern = PosGaussian 0.2
occupancy0 = Map.fromList $ zip (allTrackPos track) [0..]
p0         = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure sZ sZ
  where sZ = take 5 (repeat 0)

streamPFile :: FilePath -> World -> Double -> 
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
          occ <- readTVar (world^.occupancy)
          let posField = posToField track pos' kern
          writeTVar (world^.pos) pos'
          writeTVar (world^.trackPos)  posField
          writeTVar (world^.occupancy) (updateField (+) occ posField))
  
    
streamSpikes :: FilePath -> World -> Double -> IO ()
streamSpikes trodeFile world fileT0 = do
  fSpikes    <- BSL.readFile trodeFile
  (Right fi) <- getFileInfo trodeFile
  runEffect $
    (dropResult (produceTrodeSpikes "test" fi fSpikes)) >->
    relativeTimeCatDelayedBy spikeTime fileT0 >->
    fanoutSpikeToCells world

fanoutSpikeToCells :: World -> Consumer TrodeSpike IO r
fanoutSpikeToCells w = forever $ do
  spike <- await
  posF <- lift (atomically $ readTVar (w^.trackPos))
  lift $ forM_ (w^.placeCells)
    (\pc -> incorporateSpike pc posF spike)

incorporateSpike :: TVar PlaceCell -> Field Double -> TrodeSpike -> IO ()
incorporateSpike cell' posF spike = atomically $ do
  pc    <- readTVar cell'
  writeTVar cell' $ stepField pc posF spike

main :: IO ()
main = do
  [mwlP,mwlTT,mwlClust,tOffset] <- getArgs
  Right clustMap <- getClusters mwlClust mwlTT
  world <- world0 clustMap
  
  print "Ok"

{- Got this from OldMWL.Parse
dropResult ::  (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()
-}

usageMessage :: String
usageMessage = "watchfield pathToMWLp pathToMWLtt pathToMWLbounds startTime"