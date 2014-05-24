module Main where

import Control.Applicative
import Data.Map.KDMap
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

drawPoint :: Point2 -> Picture
drawPoint (Point2 x y w) =
  translate (realToFrac x) (realToFrac y) $ circle 3.0

data World = World { mainMap   :: KDMap Point2 Int
                   , selection :: Maybe (Point2, Int)
                   }

world0 :: World
world0 = World KDEmpty Nothing

drawTree :: KDMap Point2 a -> Picture
drawTree = Pictures . map (drawPoint) . keys

drawSelection :: Maybe (Point2,Int) -> [Picture]
drawSelection Nothing = []
drawSelection (Just ((Point2 x y w),i)) = [translate (realToFrac x) (realToFrac y) $ circleSolid 3.0]

drawWorld :: World -> IO Picture
drawWorld w = return $ Pictures (drawTree (mainMap w) :
                                 drawSelection (selection w))

fTime :: Float -> World -> IO World
fTime _ w = return w

fInputs :: Event -> World -> IO World
fInputs (EventKey (MouseButton LeftButton) Up _ (x,y)) w =
  return $ w { mainMap =
                  insert 0 (Point2 (realToFrac x) (realToFrac y) 1.0) 10 (mainMap w)}
fInputs (EventKey (MouseButton RightButton) Up _ (x,y)) w =
  return $ w { selection = closest (Point2 (realToFrac x) (realToFrac y) 5) (mainMap w)}
fInputs (EventKey (MouseButton MiddleButton) Up _ (x,y)) w =
  return $ w { selection = Nothing
             , mainMap = maybe (mainMap w)
                         (\p -> delete (fst p) (mainMap w))
                         (closest (Point2 (realToFrac x) (realToFrac y) 1.0) (mainMap w))
             }

fInputs _ w = return w

------------------------------------------------------------------------------
main :: IO ()
main = playIO (InWindow "KDMap" (500,500) (100,100))
       white 30 world0 drawWorld fInputs fTime
