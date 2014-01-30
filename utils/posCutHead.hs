{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.Parse -- meaning: ParseSpike...
import Data.Ephys.OldMWL.Header
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParsePxyabw

import System.IO
import System.Environment
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Pipes as P
import Pipes ((>->))
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as P hiding (filter,map)
import Data.Data
import qualified Pipes.Binary     as P
import Safe
import Control.Lens
import System.Console.CmdArgs

data DropCmd = DropCmd
               { inFile    :: FilePath
               , outFile   :: FilePath
               , startTime :: Double
               , restartParamsIds :: Bool
               } deriving (Show,Data,Typeable)
dropCmd :: DropCmd
dropCmd =
  DropCmd { inFile           = def &= typFile     &= help "Input file"
          , outFile          = def &= typFile     &= help "Output file"
          , startTime        = def &= typ "TIME"  &= help "Experiment time to start at"
          , restartParamsIds = False &= help "Set first kept id to 0 for pxyabw files"
          }
  
main :: IO ()
main = do
  DropCmd inName outName tStart resetIds <- cmdArgs dropCmd
  let ext    = reverse . takeWhile (/= '.') . reverse
  loadRawMWL inName >>= \r -> case r of
      Left e -> putStrLn $ "Parse error for " ++ outName ++ " : " ++ e
      Right (_,remaining) -> do
        inFileB <- BSL.readFile inName
        withFile outName WriteMode $ \outFileH -> do
          let headerSize = BSL.length inFileB - BSL.length remaining
          BSL.hPutStr outFileH $ BSL.take headerSize inFileB
          case ext inName of
                "p" -> do
                  fi' <- getFileInfo inName
                  case fi' of
                    Left e -> putStrLn $ "Error opening file " ++ inName ++ " : " ++ e
                    Right fi -> do
                      _ <- P.runEffect $ P.for source 
                           (P.liftIO . BSL.hPutStr outFileH)
                      return ()
                        where
                          p pos = pos^.mwlPosTime >= tStart
                          source :: P.MonadIO m => P.Producer BSL.ByteString m ()
                          source = do
                            _ <- P.decodeMany (P.fromLazy remaining) >->
                                 P.map snd >->
                                 P.filter p >->
                                 P.for P.cat P.encode >->
                                 P.map BSL.fromStrict
                            return ()
                "tt" -> do
                  fi' <- getFileInfo inName
                  case fi' of
                    Left e -> putStrLn $ "Error opening file " ++ inName ++ " : " ++ e
                    Right fi -> do
                      _ <- P.runEffect $ P.for source $ (P.liftIO . BSL.hPutStr outFileH)
                      return ()
                        where
                          source :: P.MonadIO m => P.Producer BSL.ByteString m ()
                          source = do
                            _ <- P.decodeGetMany (parseSpike fi) (P.fromLazy remaining) >->
                                 P.map snd >->
                                 P.filter (\spike -> mwlSpikeTime spike >= tStart) >->
                                 encodeSpike >->
                                 P.map BSL.fromStrict
                            return ()
                          encodeSpike :: P.MonadIO m => P.Pipe MWLSpike BS.ByteString m r
                          encodeSpike = P.for P.cat (\s -> P.encodePut (writeSpike fi s))
                "pxyabw" -> do
                  _ <- P.runEffect $ P.for source $ (P.liftIO . BSL.hPutStr outFileH)
                  return ()
                    where
                      source :: P.MonadIO m => P.Producer BSL.ByteString m ()
                      source = do
                        dropResult (P.decodeGetMany (parsePxyabw) (P.fromLazy remaining)) >->
                          P.map snd >->
                          P.filter (\spikeParms -> mwlSParmsTime spikeParms >= tStart) >->
                          bumpIdMaybe resetIds 0 >->
                          encodeSpikeParms >->
                          P.map BSL.fromStrict
                      encodeSpikeParms :: P.MonadIO m => P.Pipe MWLSpikeParms BS.ByteString m r
                      encodeSpikeParms = P.for P.cat (\s -> P.encodePut (writeSpikeParms s))
                _ -> putStrLn $ "Not yet supporting filtering for extension: " ++ ext inName

bumpIdMaybe :: (Monad m) => Bool -> Integer ->P.Pipe MWLSpikeParms MWLSpikeParms m ()
bumpIdMaybe doBump ind = do
  spikeParms <- P.await
  P.yield $ if doBump
          then spikeParms { mwlSParmsID = ind }
          else spikeParms
  bumpIdMaybe doBump (ind + 1)

validateArgs :: [String] -> Maybe (FilePath,FilePath, Double)
validateArgs [inName,outName,tStartS] =
  maybe Nothing (Just . (inName,outName,)) $ readMay tStartS
validateArgs _ = Nothing
    
printUsage :: IO ()
printUsage = putStrLn "posCutHead inFilename outFilename tStart"

