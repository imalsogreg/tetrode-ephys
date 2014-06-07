module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl')
import Data.Map.KDMap
import Data.Monoid
import Data.Vector ((!))
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Pipes
import qualified Pipes.Prelude as PP

import Data.Ephys.Spike
import Data.Ephys.OldMWL.Parse

drawPoint :: Point2 -> Picture
drawPoint (Point2 x y w) =
  translate (realToFrac x) (realToFrac y) $ circle (log (realToFrac w) / 10000000)

data World = World { mainMap   :: KDMap Point2 Int
                   , selection :: Maybe (Point2, Int)
                   , time      :: Double
                   , spikeChan :: TChan TrodeSpikes
                   }

instance Monoid Int where
  mempty = 0
  mappend = (+)

world0 :: Producer TrodeSpike IO () -> World
world0 p = World KDEmpty Nothing 4492 p

drawTree :: KDMap Point2 a -> Picture
drawTree = Pictures . map (drawPoint) . keys

drawSelection :: Maybe (Point2,Int) -> [Picture]
drawSelection Nothing = []
drawSelection (Just ((Point2 x y w),i)) = [translate (realToFrac x) (realToFrac y) $ circleSolid (realToFrac w)]

drawWorld :: World -> IO Picture
drawWorld w = return $ scale 3000000 3000000 $
              Pictures (drawTree (mainMap w) : drawSelection (selection w))

fTime :: Float -> World -> IO World
fTime t w = do
  let tNext = time w + realToFrac t
  spikes <- while (\s -> spikeTime s < tNow) 
  let spikePoints s = (spikeAmplitudes s ! 0, spikeAmplitudes s ! 1)
      toPoint (x,y) = Point2 (realToFrac x) (realToFrac y) 1.0
      points = map (toPoint . spikePoints) spikes
      newMap = foldl' (\m p -> add m 0.00001 p 10) (mainMap w) points
--  print $ points
  return w { mainMap = newMap,
             time = tNext
           }


------------------------------------------------------------------------------
fInputs :: Event -> World -> IO World
fInputs (EventKey (MouseButton b) Up _ (x,y)) w
  | b == LeftButton =
    let newMap = add (mainMap w) 20.0 (Point2 (realToFrac x) (realToFrac y) 3.0) 10
    in  return w { mainMap = newMap }
  | otherwise = return w
fInputs _ w = return w


------------------------------------------------------------------------------
main :: IO ()
main = do
  let fn = "/home/greghale/Data/caillou/112812clip2/1628/1628.tt"
  let p = produceTrodeSpikesFromFile fn 16
  _ <- async $ prod
  playIO (InWindow "KDMap" (500,500) (100,100))
    white 30 (world0 fi bytes) drawWorld fInputs fTime
