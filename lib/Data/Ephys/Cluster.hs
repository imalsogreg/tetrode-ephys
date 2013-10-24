{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.Cluster where

import Data.Ephys.Spike
import Data.Vector as V

import Control.Lens

type ChanInd = Int
type Polygon = [(Double,Double)]


data CartBound = CartBound { _cartXChan :: ChanInd
                           , _cartYChan :: ChanInd
                           , _cartPolygon :: Polygon
                           }
                      deriving (Eq, Show)

$(makeLenses ''CartBound)
                               
data ClusterMethod = ClustCartBound 
                   | ClustPolarBound
                   | ClustSoftCartesian
                   | ClustIntersection [ClusterMethod]
                   | ClustUnion [ClusterMethod]
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
{-
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
-}