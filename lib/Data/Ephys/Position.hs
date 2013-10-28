{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Position where

import Data.Ephys.EphysDefs

import Control.Lens

data Location = Location {_x :: Double, _y :: Double, _z :: Double}
              deriving (Eq, Show)

$(makeLenses ''Location)

data Angle = Angle {_yaw :: Double, _pitch :: Double, _roll :: Double}
           deriving (Eq, Show)

$(makeLenes ''Location)

data Position = Position {_location :: Location, _angle :: Angle}
              deriving (Eq, Show)
                       
$(makeLenses ''Position)

