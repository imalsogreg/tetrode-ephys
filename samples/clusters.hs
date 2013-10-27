{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParseClusterFile

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
    Right clustMap -> case Map.lookup 1 clustMap of
      Nothing -> error "Couldn't find cluster 1"
      Just cm -> do
{-        len <- PP.length 
               (dropResult (produceTrodeSpikes "01" fi tt) >-> 
                PP.filter (spikeInCluster cm)) -}
        --print cm
        print $ spikeInCluster cm sTest
--        runEffect $ dropResult (produceTrodeSpikes "01" fi tt) >-> PP.take 1 >->
--          PP.filter (spikeInCluster cm) >-> PP.print
        --        runEffect $ dropResult (produceTrodeSpikes "01" fi tt) >-> PP.print
--        print len