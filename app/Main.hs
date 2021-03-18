module Main
  where

import Data.Map                        ((!))
import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers           (amplitude)
import Synthesizer.Modifiers.Envelopes
import Synthesizer.Oscillator
import Synthesizer.Structure

import Notes.Default

amplitudeEnvelope = Envelope 0.2 0.3 0.0 0.0
amplitudeAmount = 32767

mediumPrioritySignal :: SynSound
mediumPrioritySignal = SynSound [
        Channel [
            applyEnvelope amplitudeEnvelope $ SoundEvent 0.0 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "C3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 0.5 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "D3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 1.0 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "E3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 1.5 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "C3")),

            applyEnvelope amplitudeEnvelope $ SoundEvent 2.0 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "C3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 2.5 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "D3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 3.0 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "E3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 3.5 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "C3")),

            applyEnvelope amplitudeEnvelope $ SoundEvent 4.0 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "E3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 4.5 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "F3")),
            applyEnvelope amplitudeEnvelope $ SoundEvent 5.0 0.5 (amplitude amplitudeAmount . sineOscillator (notes440 ! "G3"))
        ]
    ]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
