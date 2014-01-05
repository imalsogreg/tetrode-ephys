{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Ephys.Timeseries.Resample where

import Data.Ephys.Timeseries.Types

import Data.Map
import Safe
import GHC.Generics

data PaddingRule = PaddingClip | PaddingZero
                 deriving (Eq, Show, Generic)

data ShiftRule   = ShiftInterp | ShiftNearest | ShiftMostRecent
                 deriving (Eq, Show, Generic)

samplingRate :: Timeseries a -> Int
samplingRate (Timeseries (tStart,tEnd) d) =
  length (tEnd - tStart)

inInterval :: Double -> Timeseries a -> Bool
inInterval t ts = t < ts^.tsInterval.fst || t > ts.tsInterval.snd

tsIndInt :: Timeseries a -> Double -> Maybe Int
tsIndInt ts t
  | not (inInterval t ts) = Nothing
  | otherwise = floor $ (t - ts^.tsInterval.fst)/

sample :: (Real a) => PaddingRule
       -> ShiftRule
       -> Double
       -> Timeseries a
       -> a
sample padR shiftR t ts
  | not (inInterval t ts) && padR == PaddingZero = 0

subWindow :: (Real a) =>
             PaddingRule ->
             ShiftRule ->
             Double ->
             Double ->
             Timeseries a ->
             Timeseries a
subWindow padR shiftR tStart tEnd ts =
  