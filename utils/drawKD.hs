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
import Pipes.RealTime
import qualified Pipes.Prelude as PP
import System.Environment

import Data.Ephys.Spike
import Data.Ephys.OldMWL.Parse

drawPoint :: Point2 -> Picture
drawPoint (Point2 x y w) =
  translate (realToFrac x) (realToFrac y) $ circle (log (realToFrac w) / 3000000)

data World = World { mainMap   :: KDMap Point2 Int
                   , selection :: Maybe (Point2, Int)
                   , time      :: Double
                   , spikeChan :: TChan TrodeSpike
                   }

instance Monoid Int where
  mempty = 0
  mappend = (+)

world0 :: TChan TrodeSpike -> World
world0 c = World KDEmpty Nothing 4492 c

drawTree :: KDMap Point2 a -> Picture
drawTree = Pictures . map (drawPoint) . keys

drawSelection :: Maybe (Point2,Int) -> [Picture]
drawSelection Nothing = []
drawSelection (Just ((Point2 x y w),i)) = [translate (realToFrac x) (realToFrac y) $ circleSolid (realToFrac w)]

drawWorld :: World -> IO Picture
drawWorld w = return $ translate (-200) (-200) $ scale 2000000 2000000 $
              Pictures (drawTree (mainMap w) : drawSelection (selection w))

flushChan :: TChan a -> STM [a]
flushChan c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
                 e <- readTChan c
                 go (e:acc)

fTime :: Float -> World -> IO World
fTime t w = do
  let tNext = time w + realToFrac t
      c = spikeChan w
  spikes <- atomically $ flushChan c
  let spikePoints s = (spikeAmplitudes s ! 0, spikeAmplitudes s ! 1)
      toPoint (x,y) = Point2 (realToFrac x) (realToFrac y) 1.0
      points = map (toPoint . spikePoints) spikes
      newMap = foldl' (\m p -> add m 0.000007 p 10) (mainMap w) points
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
  (f:_) <- getArgs
  let fn = "/home/greghale/Data/caillou/112812clip2/" ++
           f ++ "/" ++ f ++ ".tt"
  c <- newTChanIO
  _ <- async . runEffect $ produceTrodeSpikesFromFile fn 16
       >-> relativeTimeCat (\s -> spikeTime s - 4492)
       >-> (forever $ await >>= lift . atomically . writeTChan c)
  playIO (InWindow "KDMap" (500,500) (100,100))
    white 30 (world0 c) drawWorld fInputs fTime
