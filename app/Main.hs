module Main where

import Data.Map                        ((!))
import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers           (amplitude)
import Synthesizer.Modifiers.Envelopes
import Synthesizer.Oscillator
import Synthesizer.Structure

import Notes.Default

volume = 32767

mediumPrioritySignal :: SynSound
mediumPrioritySignal = SynSound [
        Channel (playChord 0.0 2.0 (volume / 5) ('C', 3))
    ]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
