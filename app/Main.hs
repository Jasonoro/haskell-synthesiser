module Main where

import Synthesizer.Encoders.Wav
import Synthesizer.Oscillator
import Synthesizer.Structure

import Notes.Default

-- 32767

mediumPrioritySignal :: SynSound
mediumPrioritySignal = SynSound [
        Channel [
            SoundEvent 0 1000 (map (round . (*32767)) $ sineOscillator 1)
        ]
    ]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
