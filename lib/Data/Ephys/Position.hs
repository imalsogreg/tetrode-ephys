{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Position where

import Data.Ephys.EphysDefs

import Data.Graph
import Control.Lens
import Pipes
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
                         , _lastGoodTime   :: ExperimentTime
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
stepPos p0 t loc ang conf =
  Position t loc ang heading' speed' conf hHist' sHist' lastGoodTime'
    where dt = t - p0^.posTime
          supressThis   = conf < ConfSure   || locDist (p0^.location) loc > maxFrameDiff
          takeEvenIfFar = conf > ConfUnsure && t - (p0^.posTime) > posUnstickTime
          loc'           = if not supressThis || takeEvenIfFar
                           then loc
                           else (p0^.location)
          lastGoodTime'  = if conf == ConfSure then t else (p0^.lastGoodTime)
          (dx,dy,_)      = locDiff (p0^.location) loc'
          instantHeading = atan2 dy dx  --TODO: Yep.
          hHist'         = instantHeading : init (p0^.headingHistory)
          heading'       = circMean hHist'
          instantSpeed   = locDist (p0^.location) loc' / dt
          sHist'         = (clipMax 100 instantSpeed) : init (p0^.speedHistory)
          speed'         = mean sHist'
          posUnstickTime = 1
          maxFrameDiff   = 0.1

producePos :: (Monad m) => Pipe Position Position m r
producePos = await >>= go
    where go p0 = do
            p <- await
            let p' = stepPos p0 (p^.posTime) (p^.location)
                     (p^.angle) (p^.posConfidence)
            yield p'
            go p'

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