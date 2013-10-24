module Data.Ephys.OldMWL.ParseClusterFile where

import Data.Ephys.Cluster

import Text.Parsec
import Control.Applicative ((<$),(<*),(*>), liftA)
import Data.Char           (chr)

parseClusters :: String -> Either ParseError Cluster
parseClusters = ...