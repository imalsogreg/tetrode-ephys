{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Position where

import Data.Ephys.EphysDefs

import Data.Graph
import Control.Lens
import qualified Data.Trees.KdTree as KD

data Location = Location {_x :: Double, _y :: Double, _z :: Double}
              deriving (Eq, Ord, Show)

data Angle = Angle {_yaw :: Double, _pitch :: Double, _roll :: Double}
           deriving (Eq, Ord, Show)
{-
data Heading = Heading { _theta :: Double}
               deriving (Eq, Show)

data Speed = Speed { _mps :: Double }
           deriving (Eq, Show)
-}
data PosConf = ConfNone | ConfUnsure | ConfSure
             deriving (Eq, Ord, Show)

-- Full 3D position data.  For position relative to a linear track, see TrackPos
data Position = Position { _posTime        :: ExperimentTime
                         , _location       :: Location
                         , _angle          :: Angle
                         , _heading        :: Double
                         , _speed          :: Double
                         , _posConfidence  :: PosConf
                         , _headingHistory :: [Double]
                         , _speedHistory   :: [Double]
                         }
              deriving (Eq, Ord, Show)

$(makeLenses ''Location)
$(makeLenses ''Angle)
$(makeLenses ''Position)

stepPos :: Position
        -> ExperimentTime
        -> Location
        -> Angle
        -> PosConf
        -> Position
stepPos p t loc ang conf =
  Position t loc ang heading' speed' conf hHist' sHist'
  where dt = t - p^.posTime
        (dx,dy,_) = locDiff (p^.location) loc
        instantHeading = atan2 dy dx  --TODO: is this right?
        hHist'         = instantHeading : init (p^.headingHistory)
        heading'       = circMean hHist'
        instantSpeed   = locDist (p^.location) loc / dt
        sHist'         = (clipMax 100 instantSpeed) : init (p^.speedHistory)
        speed'         = mean sHist'

clipMax :: Double -> Double -> Double
clipMax m a = if a > m then m else a

locDiff :: Location -> Location -> (Double,Double,Double)
locDiff a b = (dx, dy, dz)
  where dx = b^.x - a^.x
        dy = b^.y - a^.y
        dz = b^.z - a^.z

locSqDist :: Location -> Location -> Double
locSqDist a b = (b^.z - a^.z)^(2::Int)
                + (b^.y - a^.y)^(2::Int)
                + (b^.x - a^.x)^(2::Int)

locDist :: Location -> Location -> Double
locDist a b = sqrt $ locSqDist a b

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

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral . length $ xs)

circMean :: [Double] -> Double
circMean angs =
  let mrvX = mean . map cos $ angs
      mrvY = mean . map sin $ angs
  in atan2 mrvY mrvX  -- TODO - is this right?