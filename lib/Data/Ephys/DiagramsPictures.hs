{-# LANGUAGE TypeFamilies #-}

module Data.Ephys.DiagramsPictures where

------------------------------------------------------------------------------
import Control.Lens hiding ((#))
import Codec.Picture
------------------------------------------------------------------------------
import Diagrams.Prelude hiding (location)
import Diagrams.Coordinates
import Diagrams.TwoD.Types
import Diagrams.Backend.Rasterific
------------------------------------------------------------------------------
import Data.Ephys.TrackPosition
import Data.Ephys.Position

------------------------------------------------------------------------------
ratPicture :: Position -> Diagram B R2
ratPicture p = circle 0.1 & trackToScreen &
               translate (R2 (p^.location.x) (p^.location.y))

trackToScreen :: (Transformable t, V t ~ R2) => t -> t
trackToScreen = scale 300

trackBinPathDilated ::
  TrackBin -> Double -> (Double, Double) -> [P2]
trackBinPathDilated trackB dilateLen (bottomFrac, topFrac) =
  case trackB^.binCaps of
    CapFlat (inAngle, outAngle) ->
      let w = trackB^.binWid
          binStart    = trackB^.binA
          binEnd      = trackB^.binZ
          inNudgeBot  = w/2 * sin inAngle  * bottomFrac
          inNudgeTop  = w/2 * sin inAngle  * topFrac
          outNudgeBot = w/2 * sin outAngle * bottomFrac
          outNudgeTop = w/2 * sin outAngle * topFrac
          lenMid      = (binEnd + binStart)/2
          drawStart   = (binStart -lenMid) * dilateLen + lenMid
          drawEnd     = (binEnd - lenMid)  * dilateLen + lenMid
          backLow     = (drawStart + inNudgeBot, w/2 * bottomFrac)
          backHigh    = (drawStart + inNudgeTop, w/2 * topFrac)
          frontLow    = (drawEnd + outNudgeBot, w/2 * bottomFrac)
          frontHigh   = (drawEnd + outNudgeTop, w/2 * topFrac)
      in trackToScreen . rotate (trackB^.binDir + pi @@ rad) $ -- Why the pi? 
           (map (p2)
           [backLow,frontLow,frontHigh,backHigh])
    _ -> error "Unimplemented cap style"

trackBinCenter :: TrackBin -> P2
trackBinCenter b = trackToScreen $ p2 (b^.binLoc.x, b^.binLoc.y)

trackDiagram :: Track -> Diagram B R2
trackDiagram t = position . map (\b -> (trackBinCenter b, trackBinDiagram b)) .
                 _trackBins $ t

------------------------------------------------------------------------------
trackOutline :: Track -> Diagram B R2
trackOutline t = position $ map binOutline (t^.trackBins)
  where binOutline b =
          let [pA,pB,pC,pD] = trackBinPathDilated b 1.015 (-0.5,0.5)
              strokes       = strokeLocLine (fromVertices [pA,pB]) <>
                              strokeLocLine (fromVertices [pC,pD])
          in  (trackBinCenter b, strokes)

trackBinDiagram :: TrackBin -> Diagram B R2
trackBinDiagram b =
  let ps@(p:_) = trackBinPathDilated b 0.8 (-0.5,0.5)
  in showOrigin . translate (r2 $ unp2 p) strokeLoop . closeLine .
     fromVertices $ ps

testDraw :: Diagram B R2 -> IO ()
testDraw d = writePng "/home/greghale/test.png" (renderDia Rasterific opts d)
  where opts = RasterificOptions Absolute

testTrack = circularTrack (0,0) 0.57 0.5 0.25 0.2

testBin = head $ testTrack^.trackBins
