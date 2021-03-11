module Main where

import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers    (amplitude, roundToSample)
import Synthesizer.Oscillator
import Synthesizer.Structure

import Notes.Default

mediumPrioritySignal :: SynSound
mediumPrioritySignal = SynSound [
        Channel [
            SoundEvent 0 1 (roundToSample . amplitude 32767 . sineOscillator 600)
        ]
    ]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
