module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Data.Ephys.Position
import Data.Ephys.TrackSpec

defaultState :: GameState
defaultState = GameState False False False False False (0,0)
               (SpecEdge { pointA = Location 0  0  0
                         , ctrlA  = Location 0  30 0
                         , pointB = Location 30  0 0
                         , ctrlB  = Location 30 30 0
                         })

data GameState = GameState { selectedPA  :: Bool
                           , selectedCA  :: Bool
                           , selectedPB  :: Bool
                           , selectedCB  :: Bool
                           , mouseDown   :: Bool
                           , oldMousePos :: (Float,Float)
                           , specEdge    :: SpecEdge
                           }

fInput :: Event -> GameState -> GameState
fInput (EventMotion (x',y')) s
  | mouseDown s = (movePoints s x' y') {oldMousePos = (x',y')}
  | otherwise   = s                    {oldMousePos = (x',y')}
fInput (EventKey (MouseButton LeftButton) Down _ (mX,mY)) s =
  let (sPA,sCA,sPB,sCB) = nearbyPoints s mX mY
  in s { selectedPA  = sPA
       , selectedCA  = sCA
       , selectedPB  = sPB
       , selectedCB  = sCB
       , mouseDown   = True
       , oldMousePos = (mX,mY)
       }
fInput (EventKey (MouseButton _) Up _ _) s = s {mouseDown = False}
fInput (EventKey _ _ _ _) s = s
fInput (EventResize _) s = s

f2d :: Float -> Double
f2d = realToFrac
      
d2f :: Double -> Float
d2f  = realToFrac

movePoints :: GameState -> Float -> Float -> GameState
movePoints gs@(GameState sPA sCA sPB sCB _ oldP (SpecEdge pA cA pB cB)) x' y' =
  let dCoord :: Location -> Bool -> Location
      dCoord l@(Location x0 y0 z0) b =
        if   b
        then Location (x0 + f2d x' - f2d oldX) (y0 + f2d y' - f2d oldY) z0
        else l
      (oldX, oldY) = oldP
  in  gs { specEdge = SpecEdge (dCoord pA sPA) (dCoord cA sCA)
                      (dCoord pB sPB) (dCoord cB sCB) }

nearbyPoints :: GameState -> Float -> Float -> (Bool,Bool,Bool,Bool)
nearbyPoints (GameState _ _ _ _ _ _ (SpecEdge pA' cA' pB' cB')) pX' pY'
  | pointPointDist pA' p < 5 = (True,False,False,False)
  | pointPointDist pB' p < 5 = (False,False,True,False)
  | pointPointDist cA' p < 5 = (False,True,False,False)
  | pointPointDist cB' p < 5 = (False,False,False,True)
  | linePointDist (pA', pB') p  < 5 = (True,True,True,True)
  | linePointDist (pA', cA') p  < 5 = (True,True,False,False)
  | linePointDist (pB', cB') p  < 5 = (False,False,True,True)
  | otherwise = (False,False,False,False)
  where p = Location (realToFrac pX') (realToFrac pY') 0  


------------------------------------------------------------------------------
linePointDist :: (Location,Location) -> Location -> Double
linePointDist
  (l0@(Location x0 y0 _), l1@(Location x1 y1 _)) l@(Location pX pY _) =
  let a = Location (pX  - x0) (pY  - y0) 0
      b = Location (x1 - x0) (y1 - y0) 0
      aProjLen = dotProd a (unitVect b)
      aProj    = vectScale (unitVect b) aProjLen
  in if   aProjLen < 0
     then pointPointDist l l0
     else if   aProjLen > pointPointDist l0 l1
          then pointPointDist l l1
          else vectLength $ vectSum aProj (vectScale a (-1))

pointPointDist :: Location -> Location -> Double
pointPointDist a b = vectLength $ vectDiff a b

dotProd :: Location -> Location -> Double
dotProd (Location x0 y0 z0) (Location x1 y1 z1) =
  realToFrac $ (x0*x1)+(y0*y1)+(z0*z1)

unitVect :: Location -> Location
unitVect v = vectScale v (1/vectLength v)

sq :: Double -> Double
sq = (^(2::Int))

vectLength :: Location -> Double
vectLength (Location x0 y0 z0) = sqrt (sq x0 + sq y0 + sq z0)

vectScale :: Location -> Double -> Location
vectScale (Location x0 y0 z0) s = Location (x0*s) (y0*s) (z0*s)

vectSum :: Location -> Location -> Location
vectSum (Location x0 y0 z0) (Location x1 y1 z1) =
  Location (x0+x1) (y0+y1) (z0+z1)

vectDiff :: Location -> Location -> Location
vectDiff a b = vectSum a (vectScale b (-1))

------------------------------------------------------------------------------
main :: IO ()
main = play (InWindow "spline" (500,500) (100,100))
       white 30 defaultState fDraw fInput fTime

fTime :: Float -> GameState -> GameState
fTime _ s = s

------------------------------------------------------------------------------
fDraw :: GameState -> Picture
fDraw gs@(GameState sPA sCA sPB sCB _ mPos e@(SpecEdge pA cA pB cB)) =
  Pictures [mainLine,lineA,lineB,endHandles, sPoints]
  where
    (nPA,nCA,nPB,nCB) = nearbyPoints gs (fst mPos) (snd mPos)
    pt (Location x0 y0 _) = (d2f x0, d2f y0)
    handleBox p cl nr = selectableColor blue cl nr .
                        translate (fst p) (snd p) $
                        rectangleWire 10 10
    (selMain, nearMain)  = (sPA && sPB, nPA && nPB)
    (selA,    nearA)     = (sPA && sCA, nPA && nCA)
    (selB,    nearB)     = (sPB && sCB, nPB && nCB)
    mainLine = selectableColor green selMain nearMain $ line [pt pA, pt pB]
    lineA    = selectableColor green selA    nearA    $ line [pt pA, pt cA]
    lineB    = selectableColor green selB    nearB    $ line [pt pB, pt cB]
    endHandles = Pictures [handleBox (pt pA) sPA nPA
                          ,handleBox (pt cA) sCA nCA
                          ,handleBox (pt pB) sPB nPB
                          ,handleBox (pt cB) sCB nCB]
    sPoints = Pictures $
              map (\(cX,cY) -> color green . translate cX cY $ circle 3)
              (splinePoints e)

selectableColor :: Color -> Bool -> Bool -> Picture -> Picture
selectableColor c False False = color c
selectableColor c False True  = color (light $ light c)
selectableColor c True  _     = color (dark  $ dark c)

splinePoints :: SpecEdge -> [(Float,Float)]
splinePoints e = map pt $ map (unlengthedSplineInterp e) [0,0.05 .. 1]
  where pt (Location x0 y0 _) = (realToFrac x0, realToFrac y0)
