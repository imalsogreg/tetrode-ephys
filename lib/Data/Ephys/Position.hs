{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Position where

import Data.Ephys.EphysDefs

import Control.Lens

data Location = Location {_x :: Double, _y :: Double, _z :: Double}
              deriving (Eq, Show)

$(makeLenses ''Location)

data Angle = Angle {_yaw :: Double, _pitch :: Double, _roll :: Double}
           deriving (Eq, Show)

$(makeLenses ''Angle)

data Heading = Heading {_dxdt :: Double, _dydt :: Double, _dzdt :: Double}
               deriving (Eq, Show)
$(makeLenses ''Heading)

data PosConf = Unsure | Sure
             deriving (Eq, Ord, Show)

data Position = Position {_location :: Location
                         , _angle :: Angle
                         , _heading :: Heading
                         , _posConfidence :: PosConf
                         }
              deriving (Eq, Show)
                       
$(makeLenses ''Position)

type TrackSpec = Graph 

data LinearPos = LinearPos {