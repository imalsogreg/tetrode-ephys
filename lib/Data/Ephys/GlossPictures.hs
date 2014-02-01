{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Ephys.GlossPictures where

import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.Spike

import Graphics.Gloss
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Lens
import Text.Printf

rad2Deg :: Float -> Float
rad2Deg = (* (-180 / pi))

r2 :: Double -> Float
r2 = realToFrac

trackPosPicture :: TrackPos -> Picture
trackPosPicture (TrackPos bin bir ecc) = trackBinFrame bin Line

trackBinFrame :: TrackBin -> ([(Float,Float)] -> Picture) -> Picture
trackBinFrame b f = trackBinFrameDilated b f 1 

trackBinFrameDilated :: TrackBin -> ([(Float,Float)] -> Picture) -> Float -> Picture
trackBinFrameDilated (TrackBin _ (Location x y _) dir bStart bEnd w) picType d =
    Translate (r2 x) (r2 y) $ Rotate (rad2Deg $ r2 dir) $
    picType [(r2 bStart, (r2 w)/(-2)* d)
            ,(r2 bEnd,   (r2 w)/(-2)* d)
            ,(r2 bEnd,   (r2 w)/2* d)
            ,(r2 bStart, (r2 w)/2* d)
            ,(r2 bStart, (r2 w)/(-2)*d)
            ]

drawTrack :: Track -> Picture
drawTrack t =
  pictures $ map (flip trackBinFrame Line) (t ^. trackBins) ++ map binArrow (t^. trackBins)
  where binArrow bin = drawArrowFloat (r2 $ bin^.binLoc.x, r2 $ bin^.binLoc.y)
                       ((r2 $ bin^.binZ - bin^.binA)/2)
                       (rad2Deg . r2 $ bin^.binDir) 0.01 0.08 0.04

{-
drawArrow :: (Double,Double) -> Double -> Double -> Double -> Double -> Double -> Picture
drawArrow (baseX,baseY) mag ang thickness headLen headThickness =
  let body = Polygon [(0, - r2 thickness/2)
                     ,(r2 mag - r2 headLen, - r2 thickness/2)
                     ,(r2 mag,0)
                     ,(r2 mag - r2 headLen, r2 thickness/2)
                     ,(0, r2 thickness/2)]
      head = Polygon [(r2 mag - r2 headLen, - r2 headThickness/2)
                     ,(r2 mag,0)
                     ,(r2 mag - r2 headLen, r2 headThickness/2)]
  in Translate (r2 baseX) (r2 baseY) . Rotate (r2 ang) $ pictures [body,head]
-}

drawArrowFloat :: (Float,Float) -> Float -> Float -> Float -> Float -> Float -> Picture
drawArrowFloat (baseX,baseY) mag ang thickness headLen headThickness =
  let body = Polygon [(0, - thickness/2)
                     ,(mag - headLen, - thickness/2)
                     ,(mag,0)
                     ,(mag - headLen, thickness/2)
                     ,(0, thickness/2)]
      aHead = Polygon [(mag - headLen, - headThickness/2)
                     ,(mag,0)
                     ,(mag - headLen, headThickness/2)]
  in Translate (baseX) (baseY) . Rotate (ang) $ pictures [body,aHead]

drawTrackPos :: TrackPos -> Float -> Picture
drawTrackPos (TrackPos bin dir ecc) alpha =
  Color (setAlpha col alpha) $
  trackBinFrameDilated bin Polygon dilation
  where
    baseCol  = if dir == Outbound then blue    else red
    col      = if ecc == InBounds then baseCol else addColors baseCol green
    dilation = if ecc == InBounds then 1 else 2

drawPos :: Position -> Picture
drawPos p = drawArrowFloat
            (r2 $ p^.location.x, r2 $ p^.location.y) (r2 $ p^.speed) (rad2Deg . r2 $ p^.heading)
            0.01 0.08 0.04
    
drawField :: Field Double -> Picture
drawField field =
  pictures $ map (uncurry drawTrackPos) (Map.toList $ Map.map r2 field)

drawNormalizedField :: Field Double -> Picture
drawNormalizedField field =
  pictures $ map (uncurry drawTrackPos)
  (Map.toList $ Map.map  ((*fMax) . r2) field)
    where fMax :: Float
          fMax = r2 $ 1 / Map.foldl' max 0.1 field
                 
setAlpha :: Color -> Float -> Color
setAlpha c alpha = case rgbaOfColor c of
  (r,g,b,_) -> makeColor r g b alpha

writePos :: Position -> String
writePos pos = printf "Conf: %s  T: %f  x: %f  y: %f  (Pos)\n"
               (show $ pos^.posConfidence)(pos^.posTime)(pos^.location.x)(pos^.location.y)

writeField :: Field Double -> String
writeField field =
  printf "x: %f  y: %f  dir: %s (TrackPos)\n" tX tY tD
--  printf "head snd: %f (TrackPos)\n" headP
  where
    modePos = fst . List.head . List.sortBy (\a b -> compare (snd b) (snd a)) . Map.toList $ field
    tX = modePos^.trackBin.binLoc.x
    tY = modePos^.trackBin.binLoc.y
    tD = show $ modePos^.trackDir
--    headP = snd . List.head . Map.toList $ field
