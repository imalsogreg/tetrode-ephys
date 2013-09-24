module Data.Ephys.Cluster where

import Data.Vector as V

type ChanInd = Int
type Point = (Double,Double)
type Polygon = [Point]

data CartesianBound = CartesianBound { cartXChan :: ChanInd
                                     , cartYChan :: ChanInd
                                     , cartPolygon :: Polygon
                                     }
                      deriving (Eq, Show)
                                       
data ClusterMethod = ClustCartesianBound 
                   | ClustPolarBound
                   | ClustSoftCartesian
                   | ClustIntersection ClusterMethod ClusterMethod
                   | ClustUnion ClusterMethod ClusterMethod
                   deriving (Eq, Show)

{-
-- Transcribe this very imparative c code
int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
{
  int i, j, c = 0;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
     (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;
  }
  return c;
}
-}

pointInPolygon :: Polygon -> Point -> Bool
pointInPolygon polyPts (tx,ty) = loop 0 lastInd False where
  -- Final case
  loop i j c
    | i == V.length polyPts                          = c
    | yTest (polyPts ! i) (polyPts ! j)
                           && slopeTest =  loop (succ i) i c
    | otherwise = False
  lastInd = V.length polyPts - 1
  yTest (x0,y0) (x1,y1) =  (y1 > tY) !=
slopeTest (x0,y0) (x1,y1) 