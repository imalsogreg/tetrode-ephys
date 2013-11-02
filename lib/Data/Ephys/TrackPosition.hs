{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.TrackPosition where

import Data.Ephys.Position

import Data.Graph
import Control.Lens
import qualified Data.Trees.KdTree as KD

data TrackBin = TrackBin { _binNam :: String
                         , _binLoc :: Location
                         , _binDir :: Double -- radians
                         , _binA   :: Double --
                         , _binZ   :: Double
                         } deriving (Eq, Show)

$(makeLenses ''TrackBin)

instance KD.Point TrackBin where
  dimension b = dimension $ binLoc b
  coord n   b = coord n (binLoc b)
  dist2 b1 b2 = dist2 (b1 ^. binLoc) (b2 ^. binLoc)

data TrackSpec = TrackSpec { _keyPoints :: Graph }  -- node :: (x,y), key :: String

data Track = Track { _trackBins :: KD.KdTree TrackBin
                   , _trackWid  :: Double
                   } deriving (Eq, Show)

data TrackDirection = Outbound | Inbound
                    deriving (Eq, Ord, Show)

data TrackEccentricity = OutOfBounds | InBounds 
                       deriving (Eq, Ord, Show)

data TrackPos = TrackPos { _trackBin :: TrackBin
                         , _trackDir :: TrackDirection
                         , _trackEcc :: TrackEccentricity
                         } deriving (Eq, Show)

$(makeLenses ''TrackSpec)
$(makeLenses ''Track)
$(makeLenses ''TrackPos)

trackFromSpec :: TrackSpec 
                 -> Double -- track width in metres
                 -> Double -- bin length in meters
                 -> Track
trackFromSpec = -- TODO 
  error "Not yet implemented: track from spec" 

data PosKernel = PosDelta
               | PosGaussian Double

putOnTrack :: Track -> Position -> PosKernel -> TrackPos
putOnTrack t pos kern =
  let closestBin = KD.nearestNeighbor (t ^. trackBins)
      rotate th (x,y) =
        let xR = x * cos th - y * sin th
            yR = x * sin th + y * cos th
        in (xR,yR)
{-      rotAbout (cX,cY) th (x,y) =
        let (x0,y0) = rotate th (x,y) in
        (x0 + cX, y0 + cY) -}
      posInBinCoords =
        rotate (-1 * (closestBin ^. binDir))
        (pos ^. location.x, pos ^. location.y)
  in
  
  


circularTrack :: (Double,Double) -- (x,y) in meters
                 -> Double       -- radius in meters
                 -> Double       -- height in meters
                 -> Double       -- track width in meters
                 -> Double       -- bin length in meters
                 -> Track
circularTrack (cX,cY) r h w tau =
  Track (KD.fromList [aPoint t | t <- thetaCs]) w
  where
    fI = fromIntegral
    diam = 2*pi*r
    nPts = floor (diam / tau) :: Int
    tau' = diam / fI nPts
    thetaCs = [0, tau' .. (diam - tau')]
    aPoint :: Double -> TrackBin
    aPoint theta = TrackBin ""
                   (r * cos theta + cX) (r * sin theta + cY) h
                   (theta + pi/2)
                   (-1 * tau / 2) (tau / 2) in
  

  