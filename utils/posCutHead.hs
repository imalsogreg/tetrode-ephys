{-# LANGUAGE TupleSections #-}

module Main where

import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.Header
import Data.Ephys.OldMWL.FileInfo

import System.IO
import System.Environment
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
            inFileB <- BSL.readFile inName
            withFile outName WriteMode $ \outFileH -> do
              let headerSize = BSL.length inFileB - BSL.length remaining
              BSL.hPutStr outFileH $ BSL.take headerSize inFileB
              _ <- P.runEffect $ P.for source
                   (P.liftIO . BSL.hPutStr outFileH)
              return ()
                where
                  p pos = pos^.mwlPosTime >= tStart
                  source =
                    P.decodeMany (P.fromLazy remaining) >->
                    P.map snd >->
                    P.filter p >->
                    P.for P.cat P.encode >->
                    P.map BSL.fromStrict
    Nothing -> printUsage

validateArgs :: [String] -> Maybe (FilePath,FilePath, Double)
validateArgs [inName,outName,tStartS] =
  maybe Nothing (Just . (inName,outName,)) $ readMay tStartS
validateArgs _ = Nothing
    
printUsage :: IO ()
printUsage = putStrLn "posCutHead inFilename outFilename tStart"

type TimeFun = FileInfo -> BSL.ByteString -> Double

fileTypeFuncs :: FileName -> Maybe TimeFun
fileTypeFuncs fn =
  let ext = reverse . takeWhile (/= '.') . reverse
  in case ext fn of
    "tt" -> 
  
