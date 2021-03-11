module Synthesizer.Oscillator where

import Data.Fixed
import Numeric
import Prelude               hiding (cycle)
import Synthesizer.Structure

type WaveFunction = Double -> Double

sawToothOscillator :: Frequency -> SamplingRate -> [Double]
sawToothOscillator frequency = oscillator sawTooth frequency
  where
    sawTooth :: WaveFunction
    sawTooth x = x `mod'` frequency

sineOscillator :: Frequency -> SamplingRate -> [Double]
sineOscillator = oscillator sin

oscillator :: WaveFunction -> Frequency -> SamplingRate -> [Double]
oscillator waveFunction frequency samplingRate = waveFunction <$> [0, delta..]
  where
    sr :: Double
    sr = fromIntegral samplingRate
    cycle :: Double
    cycle = 2 * pi
    delta :: Double
    delta = (frequency * cycle) / sr
