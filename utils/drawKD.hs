module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.List (foldl')
import Data.Map.KDMap
import Data.Monoid
import Data.Vector ((!))
import Debug.Trace
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Interface.IO.Game
import Pipes
import Pipes.RealTime
import qualified Pipes.Prelude as PP
import System.Environment

import Data.Ephys.Spike
import Data.Ephys.OldMWL.Parse

drawPoint :: Double -> (Point2, SpikeTime) -> Picture
drawPoint tNow (Point2 x y w, tSpike) =
  translate (realToFrac x) (realToFrac y) $
  Pictures [ color (pointColor tNow tSpike) $ circleSolid (1e-6 + log (realToFrac w) / 3000000)
           , circle (1e-6 + log (realToFrac w) / 3000000)
           ]

pointColor :: Double -> SpikeTime -> Color.Color
pointColor tNow (SpikeTime tSpike) = Color.makeColor r g b 1
  where e tau = exp (-1 * (max dt 0) / tau)
        dt = (realToFrac tNow) - (realToFrac tSpike)
        r = e 1
        g = e 0.5
        b = e 20

newtype SpikeTime = SpikeTime { unSpikeTime :: Double }
                  deriving (Eq)

instance Monoid SpikeTime where
  mempty = SpikeTime (-1e6)
  mappend a b = SpikeTime $ (max `on` unSpikeTime) a b



data World = World { mainMap   :: KDMap Point2 SpikeTime
                   , selection :: Maybe (Point2, SpikeTime)
                   , time      :: Double
                   , spikeChan :: TChan TrodeSpike
                   }

world0 :: TChan TrodeSpike -> World
world0 c = World KDEmpty Nothing 4492 c

drawTree :: Double -> KDMap Point2 SpikeTime -> Picture
drawTree tNow = Pictures . map (drawPoint tNow) . toList

drawSelection :: Maybe (Point2,SpikeTime) -> [Picture]
drawSelection Nothing = []
drawSelection (Just ((Point2 x y w),i)) = [translate (realToFrac x) (realToFrac y) $
                                           circleSolid (realToFrac w)]

drawWorld :: World -> IO Picture
drawWorld w = return $ translate (-200) (-200) $ scale 2000000 2000000 $
              Pictures (drawTree (time w) (mainMap w) : drawSelection (selection w))

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
      times  = map (SpikeTime . spikeTime) spikes
      newMap = foldl' (\m (p,t) -> add m 0.000007 p t) (mainMap w) (zip points times)
  return w { mainMap = newMap,
             time = tNext
           }


------------------------------------------------------------------------------
fInputs :: Event -> World -> IO World
fInputs (EventKey (MouseButton b) Up _ (x,y)) w
  | b == LeftButton =
    let newMap = add (mainMap w) 20.0 (Point2 (realToFrac x) (realToFrac y) 3.0) (SpikeTime 0)
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
