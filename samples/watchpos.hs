{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.Position
import Data.Ephys.TrackPosition

import Control.Lens
import Graphics.Gloss
import qualified Data.Map as Map
import Graphics.Gloss.Interface.IO.Game

fI = fromIntegral
r2 = realToFrac

rad2Deg = (* (-180 / pi))

type World = (Position, Track, Field Double)

trackPosPicture :: TrackPos -> Picture
trackPosPicture (TrackPos bin dir ecc) = trackBinFrame bin Line

trackBinFrameDilated :: TrackBin -> ([(Float,Float)] -> Picture) -> Float -> Picture
trackBinFrameDilated (TrackBin _ (Location x y _) dir bStart bEnd w) picType d =
  Translate (r2 x) (r2 y) $ Rotate (r2 $ rad2Deg dir) $
    picType [(r2 bStart, r2 $ w/(-2)* r2 d)
            ,(r2 bEnd,   r2 $ w/(-2)* r2 d)
            ,(r2 bEnd,   r2 $ w/2* r2 d)
            ,(r2 bStart, r2 $ w/2* r2 d)
            ,(r2 bStart, r2 $ w/(-2)* r2 d)
            ]

trackBinFrame :: TrackBin -> ([(Float, Float)] -> Picture) -> Picture
trackBinFrame b f = trackBinFrameDilated b f 1

drawTrack :: Track -> Picture
drawTrack t = pictures $ map (flip trackBinFrame Line) (t ^. trackBins)

drawTrackPos :: TrackPos -> Float -> Picture
drawTrackPos (TrackPos bin dir ecc) alpha =
  Color (setAlpha col alpha) $
  trackBinFrameDilated bin Polygon dilation
  where
    baseCol  = if dir == Outbound then blue    else red
    col      = if ecc == InBounds then baseCol else addColors baseCol green
    dilation = if ecc == InBounds then 1 else 2
    
drawField :: Field Double -> Picture
drawField field =
  pictures $ map (uncurry drawTrackPos) (Map.toList $ Map.map r2 field)

drawNormalizedField :: Field Double -> Picture
drawNormalizedField field =
  pictures $ map (uncurry drawTrackPos) (Map.toList $ Map.map (r2 . (/fMax)) field)
    where fMax :: Double
          fMax = Map.foldl' max 0 field
                 
setAlpha :: Color -> Float -> Color
setAlpha c alpha = case rgbaOfColor c of
  (r,g,b,_) -> makeColor r g b alpha
    
myTrack :: Track
myTrack = circularTrack (0,0) 0.75 0 0.2 0.25

gScale :: Float
gScale = 200

main :: IO ()
main = playIO (InWindow "My Window" (400,400) (10,10))
       white
       60
       (p0,t0,f0)
       drawWorld
       (eventUpdateWorld)
       (timeUpdateWorld)
  where p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure
        t0 = myTrack
        f0 = Map.fromList [ (tp,0) | tp <- allTrackPos t0 ]

eventUpdateWorld :: Event -> World -> IO World
eventUpdateWorld (EventMotion (x',y')) (p,t,occ) =
  let p' = Position 0 (Location ((r2 x')/ r2 gScale) ((r2 y') / r2 gScale) (p^.location.z))
           (Angle 0 0 0) 0 0 ConfSure
      occ' = updateField (+) occ (posToField t p (PosGaussian 0.4))
  in return (p',t,occ')
eventUpdateWorld (EventKey _ _ _ _) w = return w
eventUpdateWorld (EventResize _) w = return w 

timeUpdateWorld :: Float -> World -> IO World
timeUpdateWorld t w = return w

drawWorld :: World -> IO Picture
drawWorld (p,t,occ) = return $ Scale gScale gScale $ pictures [drawTrack t, drawNormalizedField occ ]
