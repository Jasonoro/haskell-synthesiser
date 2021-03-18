module Synthesizer.Modifiers
  where

import Synthesizer.Structure (Sample)

-- | Change the amplitude of a sample list
amplitude :: Double     -- ^ The multiplier of the amplitude
          -> [Double]   -- ^ The list of sampled points
          -> [Double]
amplitude amount = map (* amount)
