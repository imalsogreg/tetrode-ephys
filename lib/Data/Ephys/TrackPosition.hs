{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.TrackPosition where

import Data.Ephys.Position

import Data.Graph
import Data.List (sortBy)
import qualified Data.Map as Map
import Control.Lens
import Control.Applicative ((<$>),(<*>))
--import qualified Data.Trees.KdTree as KD

data TrackBin = TrackBin { _binNam :: String
                         , _binLoc :: Location
                         , _binDir :: Double -- radians
                         , _binA   :: Double --
                         , _binZ   :: Double
                         } deriving (Eq, Show)

$(makeLenses ''TrackBin)

{- Don't store track points in a KdTree anymore.
instance KD.Point TrackBin where
  dimension b = KD.dimension $ _binLoc b
  coord n   b = KD.coord n (_binLoc b)
  dist2 b1 b2 = KD.dist2 (b1 ^. binLoc) (b2 ^. binLoc)
-}

data TrackSpec = TrackSpec { _keyPoints :: Graph }  -- node :: (x,y), key :: String

data Track = Track { _trackBins  :: [TrackBin]
                   , _trackWidth :: Double
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

allTrackPos :: Track -> [TrackPos]
allTrackPos t = [TrackPos bin dir ecc | bin <- t^.trackBins
                                      , dir <- [Outbound,Inbound]
                                      , ecc <- [InBounds,OutOfBounds]]

-- Use mapping from track bin to a to model 'fields' in general
-- ie an instantaneous occupancy field, a trial-sum occupancy
-- field, or a spike rate field
type Field a = TrackPos -> a

trackFromSpec :: TrackSpec 
                 -> Double -- track width in metres
                 -> Double -- bin length in meters
                 -> Track
trackFromSpec = -- TODO 
  error "Not yet implemented: track from spec" 

data PosKernel = PosDelta
               | PosGaussian Double

-- Turn a position into an instantaneous field
posToField :: Track -> Position -> PosKernel -> Field Double
posToField t pos kern =
    let distSq bin = locSqDist (pos^.location) (bin^.binLoc)
        binC       = trackClosestBin t pos
        tpC        = posToTrackPos t pos
        leastDist  = distSq binC
        tDir = if cos (pos^.heading - binC^.binDir) > 0 then Outbound else Inbound
        ecc b = if (abs y') > (t^.trackWidth / 2) then OutOfBounds else InBounds
          where (_,y') = relativeCoords binC (pos^.location^.x, pos^.location^.y)
        inBin bin =  x' >= (binC^.binA) && x' <= (binC^.binZ)
          where (x',_) = relativeCoords binC (pos^.location^.x, pos^.location^.y)
        trackPosValUnNormalized :: TrackPos -> Double
        trackPosValUnNormalized tp = case kern of
          PosDelta    -> if tp^.trackBin == binC
                            && tp^.trackDir == tDir
                            && tp^.trackEcc == ecc binC
                         then 1 else 0
          PosGaussian sd ->
            if (tp^.trackEcc) == ecc binC && (tp^.trackDir) == tDir
            then exp( -1 * distSq (tp^.trackBin) / (2 * sd * sd) )
            else 0
        totalVal = sum $ map trackPosValUnNormalized (allTrackPos t)
        trackPosVal tp = trackPosValUnNormalized tp / totalVal
     in trackPosVal

posToTrackPos :: Track  -> Position -> Maybe TrackPos
posToTrackPos track pos =
  let binC = trackClosestBin track pos
      (x',y') = relativeCoords binC (pos^.location^.x, pos^.location^.y)
      ecc = if (abs x') > (track^.trackWidth/2) then OutOfBounds else InBounds
      tDir = if cos (pos^.heading - binC^.binDir) > 0 then Outbound else Inbound
      inBin = x' >= (binC^.binA) && x' <= binC^.binZ in
  case inBin of
    False -> Nothing
    True  -> Just $ TrackPos binC tDir ecc

relativeCoords :: TrackBin -> (Double,Double) -> (Double,Double)
relativeCoords bin (x,y) = let th = (-1 * bin^.binDir) in
  (x * cos th - y * sin th, x * sin th + y * cos th) --TODO check rotation matrix

trackClosestBin :: Track -> Position -> TrackBin
trackClosestBin track pos =
  head $ sortBy (\b0 b1 -> compare (posBinDistSq pos b0) (posBinDistSq pos b1)) (track^.trackBins)

posBinDistSq :: Position -> TrackBin -> Double
posBinDistSq pos bin = locSqDist (bin^.binLoc) (pos^.location)

circularTrack :: (Double,Double) -- (x,y) in meters
                 -> Double       -- radius in meters
                 -> Double       -- height in meters
                 -> Double       -- track width in meters
                 -> Double       -- bin length in meters
                 -> Track
circularTrack (cX,cY) r h w tau =
  Track [aPoint t | t <- thetaCs] w
  where
    fI = fromIntegral
    diam = 2*pi*r
    nPts = floor (diam / tau) :: Int
    tau' = diam / fI nPts
    thetaCs = [0, tau' .. (diam - tau')]
    aPoint :: Double -> TrackBin
    aPoint theta = TrackBin ""
                   (Location (r * cos theta + cX) (r * sin theta + cY) h)
                   (theta + pi/2)
                   (-1 * tau / 2) (tau / 2)
  

  