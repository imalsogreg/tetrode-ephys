-----------------------------------------------------------
-- spikeWaveform.hs 
--
-- Greg Hale 2013
-----------------------------------------------------------

import Data.Array.IArray

data SpikeWaveform = SpikeWaveform {sourceTrode :: Int
                                   ,timeStamp   :: Double
                                   ,metadataID  :: Int
                                   ,waveform    :: (Array Int Float)
                                   }
                     
main = putStrLn "Hi."