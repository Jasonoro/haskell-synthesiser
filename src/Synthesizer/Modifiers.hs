module Synthesizer.Modifiers (
  applyAmplitude
  )
  where

import Synthesizer.Structure (Sample)

-- | Change the amplitude of a sample list
applyAmplitude :: Double     -- ^ The multiplier of the amplitude
               -> [Double]   -- ^ The list of sampled points
               -> [Double]
applyAmplitude amount = map (* amount)
