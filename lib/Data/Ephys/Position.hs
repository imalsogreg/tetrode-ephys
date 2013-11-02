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

data TrackBin = TrackBin { _binNam :: String
                         , _binX   :: Double
                         , _binY   :: Double
                         , _binDir :: Double -- radians
                         , _binA   :: Double --
                         , _binZ   :: Double
                         } deriving (Eq, Show)

$(makeLenses ''TrackBin)

-- TODO - risky to depend on KdTree?  Not very active package
instance KD.Point TrackBin where
  dimension   = const 2
  coord 0     = _binX
  coord 1     = _binY
  coord 2     = const 0
  coord n     = error $
                "Impossible coordinate from TrackBin: " ++ show n
  dist2 b1 b2 = (b2 ^. binX - b1 ^. binX)^(2::Int) +
                   (b2 ^. binY - b1 ^. binY)^(2::Int)

data TrackSpec = TrackSpec { _keyPoints :: Graph }  -- node :: (x,y), key :: String

data Track = Track { _trackBins :: KD.KdTree TrackBin
                   , _trackWid  :: Double
                   } deriving (Eq, Show)

data TrackDirection = Outbound | Inbound
                    deriving (Eq, Ord, Show)

data TrackPos = TrackPos { _trackBin :: TrackBin
                         , _trackDir :: TrackDirection
                         } deriving (Eq, Show)

$(makeLenses ''TrackSpec)
$(makeLenses ''Track)
$(makeLenses ''TrackPos)

trackFromSpec :: TrackSpec 
                 -> Double -- track width in metres
                 -> Double -- bin length in meters
                 -> Track
trackFromSpec = undefined

circularTrack :: (Double,Double) -- (x,y) in meters
                 -> Double       -- radius in meters
                 -> Double       -- track width in meters
                 -> Double       -- bin length in meters
                 -> Track
circularTrack (cX,cY) r w tau =
  let fI = fromIntegral
      diam = 2*pi*r
      nPts = floor (diam / tau) :: Int
      tau' = diam / fI nPts
      thetaCs = [0, tau' .. (diam - tau')]
      aPoint :: Double -> TrackBin
      aPoint theta = TrackBin ""
                     (r * cos theta + cX) (r * sin theta + cY)
                     (theta + pi/2)
                     (-1 * tau / 2) (tau / 2) in
  Track (KD.fromList [aPoint t | t <- thetaCs]) w
                     