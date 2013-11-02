{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Position where

import Data.Ephys.EphysDefs

import Data.Graph
import Control.Lens
import qualified Data.Trees.KdTree as KD

data Location = Location {_x :: Double, _y :: Double, _z :: Double}
              deriving (Eq, Show)

data Angle = Angle {_yaw :: Double, _pitch :: Double, _roll :: Double}
           deriving (Eq, Show)

data Heading = Heading {_dxdt :: Double, _dydt :: Double, _dzdt :: Double}
               deriving (Eq, Show)

data PosConf = ConfNone | ConfUnsure | ConfSure
             deriving (Eq, Ord, Show)

-- Full 3D position data.  For position relative to a linear track, see TrackPos
data Position = Position {_location       :: Location
                         , _angle         :: Angle
                         , _heading       :: Heading
                         , _posConfidence :: PosConf
                         }
              deriving (Eq, Show)

$(makeLenses ''Location)
$(makeLenses ''Angle)
$(makeLenses ''Heading)
$(makeLenses ''Position)

-- TODO - risky to depend on KdTree?  Not very active package
instance KD.Point Location where
  dimension   = const 3
  coord 0 a   = a ^. x
  coord 1 a   = a ^. y
  coord 2 a   = a ^. z
  coord n _   = error $
                "Impossible coord from Location: " ++ show n
  dist2 a1 a2 = (a2 ^. x - a1 ^. x)^(2::Int) +
                (a2 ^. y - a1 ^. y)^(2::Int) +
                (a2 ^. z - a1 ^. z)^(2::Int)