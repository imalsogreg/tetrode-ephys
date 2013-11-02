{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Position where

import Data.Ephys.EphysDefs

import Control.Lens

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

type TrackBin = 

type TrackSpec = TrackSpek { _keyPoints :: Graph }  -- node :: (x,y), key :: String

data Track = Track { _trackBin :: KDTree TrackBin
                   , _trackWid :: Double
                   } deriving (Eq, Show)

data TrackDirection = Outbound | Inbound
                    deriving (Eq, Ord, Show)

data TrackPos = TrackPos { _trackBin :: TrackBin
                         , _trackDir :: TrackDirection
                         } deriving (Eq, Show)