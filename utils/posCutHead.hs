{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.Parse -- meaning: ParseSpike...
import Data.Ephys.OldMWL.Header
import Data.Ephys.OldMWL.FileInfo

import System.IO
import System.Environment
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Pipes as P
import Pipes ((>->))
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as P hiding (filter,map)

import qualified Pipes.Binary     as P
import Safe
import Control.Lens

main :: IO ()
main = do
  args <- getArgs
  case validateArgs args of
    Just (inName,outName,tStart) -> loadRawMWL inName >>= \r -> case r of
      Left e -> putStrLn $ "Parse error for " ++ outName ++ " : " ++ e
      Right (_,remaining) -> do
        fi' <- getFileInfo inName
        case fi' of
          Left e -> putStrLn $ "Error opening file " ++ inName ++ " : " ++ e
          Right fi -> do
            let ext    = reverse . takeWhile (/= '.') . reverse
            inFileB <- BSL.readFile inName
            withFile outName WriteMode $ \outFileH -> do
              let headerSize = BSL.length inFileB - BSL.length remaining
              BSL.hPutStr outFileH $ BSL.take headerSize inFileB
              case ext inName of
                "p" -> do
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
                _ -> putStrLn $ "Not yet supporting filtering for extension: " ++ ext inName
    Nothing -> printUsage

validateArgs :: [String] -> Maybe (FilePath,FilePath, Double)
validateArgs [inName,outName,tStartS] =
  maybe Nothing (Just . (inName,outName,)) $ readMay tStartS
validateArgs _ = Nothing
    
printUsage :: IO ()
printUsage = putStrLn "posCutHead inFilename outFilename tStart"

