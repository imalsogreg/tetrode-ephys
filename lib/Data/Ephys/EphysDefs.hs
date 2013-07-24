{-# LANGUAGE BangPatterns, TypeSynonymInstances, DeriveDataTypeable #-}

module Data.Ephys.EphysDefs where

import Data.Time

type Voltage = Double

type ExperimentTime = ZonedTime