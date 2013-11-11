{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParseClusterFile

import Pipes.RealTime
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BSL
import Pipes
import qualified Pipes.Prelude as PP
import qualified Data.Map as Map

basePath :: FilePath
basePath = "/home/greghale/Data/caillou/112812/1028/"

ttFile = basePath ++ "1028.tt"
clustFile = basePath ++ "cbfile-run"

main :: IO ()
main = do 
  tt <- BSL.readFile ttFile
  Right fi <- getFileInfo ttFile
  clustsE <- getClusters clustFile ttFile
  sTest <- mySpike
  case clustsE of
    Left e -> error $ "e was: " ++ e
    Right clustMap -> case Map.lookup 1 clustMap of
      Nothing -> error "Couldn't find cluster 1"
      Just cm@(ClustIntersection polys) -> do
        len <- PP.length 
               (dropResult (produceTrodeSpikes "01" fi tt) >->
                relativeTimeCat spikeTime >->
                PP.filter (spikeInCluster cm) >-> PP.print)
        --print cm
   {-     let (ClustCartBound b) = head polys
            amps = spikeAmplitudes sTest
--        print $ (amps !! 0, amps !! 1)
--        print $ pointInPolygon (_cartPolygon b) (amps !! 0, amps !! 1)
--        print $ spikeInCluster (head polys) sTest
        runEffect $ dropResult (produceTrodeSpikes "01" fi tt) >->
          PP.filter (spikeInCluster cm) >-> PP.print
        --        runEffect $ dropResult (produceTrodeSpikes "01" fi tt) >-> PP.print -}
        print len