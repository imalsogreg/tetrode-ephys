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
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Vector ((!))
import Debug.Trace
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Interface.IO.Game
import Pipes
import Pipes.RealTime
import qualified Pipes.Prelude as PP
import System.Environment
import System.Directory
import Data.Ephys.Spike
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParseSpike

drawPoint :: Int -> Int -> Double -> (Point4, SpikeTime) -> Picture
drawPoint xInd yInd tNow (p, tSpike) =
  translate (realToFrac $ pointD p (Depth xInd)) (realToFrac $ pointD p (Depth yInd)) $
  Pictures [ color (pointColor tNow tSpike) $
             circleSolid (1e-6 + log (realToFrac $ pointW p) / 3000000)
           , circle (1e-6 + log (realToFrac $ pointW p) / 3000000)
           ]

pointColor :: Double -> SpikeTime -> Color.Color
pointColor tNow (SpikeTime tSpike) = Color.makeColor r g b 1
  where e tau = exp (-1 * (max dt 0) / tau)
        dt = (realToFrac tNow) - (realToFrac tSpike)
        r = e 0.2
        g = e 1
        b = e 20

newtype SpikeTime = SpikeTime { unSpikeTime :: Double }
                  deriving (Eq)

instance Monoid SpikeTime where
  mempty = SpikeTime (-1e6)
  mappend a b = SpikeTime $ (max `on` unSpikeTime) a b


data World = World { mainMap   :: KDMap Point4 SpikeTime
                   , selection :: Maybe (Point4, SpikeTime)
                   , time      :: Double
                   , spikeChan :: TChan TrodeSpike
                   , chanX     :: Int
                   , chanY     :: Int    
                   }

world0 :: TChan TrodeSpike -> World
world0 c = World KDEmpty Nothing 4492 c 0 1

drawTree :: Int -> Int -> Double -> KDMap Point4 SpikeTime -> Picture
drawTree xInd yInd tNow = Pictures . map (drawPoint xInd yInd tNow) . toList

drawSelection :: Int -> Int -> Double -> Maybe (Point4,SpikeTime) -> [Picture]
drawSelection _ _ _ Nothing = []
drawSelection xInd yInd tNow (Just (p,i)) = [drawPoint xInd yInd tNow (p,i)]

drawWorld :: World -> IO Picture
drawWorld w = do
  return $ translate (-200) (-200) $ scale 2000000 2000000 $
    Pictures (drawTree x y t (mainMap w) : drawSelection x y t (selection w))
  where t = time w
        x = chanX w
        y = chanY w

flushChan :: TChan a -> STM [a]
flushChan c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
                 e <- readTChan c
                 go (e:acc)

fTime :: UTCTime -> Float -> World -> IO World
fTime t0 _ w = do
  tNext <- getExperimentTime t0 4492  -- (old way) time w + realToFrac t
  let c = spikeChan w
  spikes <- atomically $ flushChan c
  let spikeCoords s = (spikeAmplitudes s ! 1, spikeAmplitudes s ! 2)
      toPoint s = Point4
                  (realToFrac $ spikeAmplitudes s ! 0)
                  (realToFrac $ spikeAmplitudes s ! 1)
                  (realToFrac $ spikeAmplitudes s ! 2)
                  (realToFrac $ spikeAmplitudes s ! 3)
                  1.0
      points = map toPoint spikes
      times  = map (SpikeTime . spikeTime) spikes
      newMap = foldl' (\m (p,t) -> add m 0.000016 p t) (mainMap w) (zip points times)
  return w { mainMap = newMap,
             time = tNext
           }


------------------------------------------------------------------------------
fInputs :: Event -> World -> IO World
fInputs (EventKey (MouseButton b) Up _ (x,y)) w
--  | b == LeftButton =
--    let newMap = add (mainMap w) 20.0 (Point4 (realToFrac x) (realToFrac y) 3.0) (SpikeTime 0)
--    in  return w { mainMap = newMap }
  | b == LeftButton =
    return $ w { chanX = (chanX w + 1) `mod` 4}
  | b == RightButton =
      return $ w { chanY = (chanY w + 1) `mod` 4}
  | otherwise = return w

fInputs _ w = return w


------------------------------------------------------------------------------
main :: IO ()
main = do
  (f:_) <- getArgs
  --let fn = "/home/greghale/Data/caillou/112812clip2/" ++
    --       f ++ "/" ++ f ++ ".tt"
  let fn = f
  fileExist <-doesFileExist fn
  when (not fileExist) $ error "File does not exist"
  c <- newTChanIO
  t0 <- getCurrentTime
  _ <- async . runEffect $ produceTrodeSpikesFromFile fn 16
       >-> relativeTimeCat (\s -> spikeTime s - 4492)
       >-> (forever $ await >>= lift . atomically . writeTChan c)
  playIO (InWindow "KDMap" (500,500) (100,100))
    white 30 (world0 c) drawWorld fInputs (fTime t0)

getExperimentTime :: UTCTime -> Double -> IO Double
getExperimentTime t0 et0 =
  (et0 +) . realToFrac . flip diffUTCTime t0 <$> getCurrentTime
