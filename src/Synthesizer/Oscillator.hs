module Synthesizer.Oscillator
  ( sawToothOscillator
  , sineOscillator
  ) where

import Data.Fixed            (mod')
import Numeric               (Floating (pi, sin))
import Prelude               hiding (cycle)
import Synthesizer.Structure (Frequency, SamplingRate)

type WaveFunction = Double -> Double

-- | Create a saw tooth oscillator with a given frequency
sawToothOscillator :: Frequency -> SamplingRate -> [Double]
sawToothOscillator frequency = oscillator sawTooth frequency
  where
    sawTooth :: WaveFunction
    sawTooth x = x `mod'` frequency

-- | Create a sine wave oscillator with a given frequency
sineOscillator :: Frequency -> SamplingRate -> [Double]
sineOscillator = oscillator sin

-- | Create a oscillator based on some wave function and a frequency
oscillator :: WaveFunction -> Frequency -> SamplingRate -> [Double]
oscillator waveFunction frequency samplingRate = waveFunction <$> [0, delta..]
  where
    sr :: Double
    sr = fromIntegral samplingRate
    cycle :: Double
    cycle = 2 * pi
    delta :: Double
    delta = (frequency * cycle) / sr
