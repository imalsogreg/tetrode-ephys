module Data.Ephys.TimeSignal where

import Data.Hashable (Hashable(..))

data FilterSpec = FilterSpec Family Response Sharpness

data Response = LowPass  Double
              | HighPass Double
              | BandPass Double Double
              | BandStop Double Double
              | Notch    Double
              deriving (Eq, Show)

instance Hashable Response where
  hashWithSalt salt a  = hashWithSalt salt (show a) 


data Sharpness = Order Integer
               | PassStopRipple Double Double
               deriving (Eq, Show)

instance Hashable Sharpness where
 hashWithSalt salt s = hashWithSalt salt (show s)


data Family = Butterworth
            | Elliptic
            deriving (Eq, Show)

instance Hashable Family where
  hashWithSalt salt f = hashWithSalt salt (show f) 