{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.OldMWL.ParsePFile where

import Data.Ephys.Position

import Control.Lens
import GHC.Int
import Pipes
import qualified Pipes.Prelude as PP



data MWLPos = MWLPos { _mwlPosTime  :: Double
                     , _mwlPosFrame :: Int
                     , _x           :: Int
                     , _y           :: Int
                     } deriving (Eq, Show)

$(makeLenses ''MWLPos)

okPFile :: FileInfo -> Bool
okPFile FileInfo{..} = hRecMode == Tracker
                       && hRecordDescr `hasField` 