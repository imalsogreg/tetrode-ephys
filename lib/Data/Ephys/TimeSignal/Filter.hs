module Data.Ephys.TimeSignal.Filter where

import Data.Hashable (Hashable(..))

-- Order of constructor arguments supposed to match natural
-- language for specifying filters, e.g. "4th order Lowpass Eliptical filter"
data FilterSpec = FilterSpec Sharpness Response Family
                deriving (Eq, Show)

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