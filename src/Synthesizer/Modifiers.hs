module Synthesizer.Modifiers where

import Synthesizer.Structure (Sample)

amplitude :: Double -> [Double] -> [Double]
amplitude amount = map (* amount)

roundToSample :: [Double] -> [Sample]
roundToSample = map round
