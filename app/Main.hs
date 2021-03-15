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
        -- ++
        -- (playChord 1.0 2.0 (volume / 5) ('C', 2))
        -- [
        --     playNote 0.0 0.5 volume ('C', 3),
        --     playNote 0.5 0.5 volume ('D', 3),
        --     playNote 1.0 0.5 volume ('E', 3),
        --     playNote 1.5 0.5 volume ('C', 3),

        --     playNote 2.0 0.5 volume ('C', 3),
        --     playNote 2.5 0.5 volume ('D', 3),
        --     playNote 3.0 0.5 volume ('E', 3),
        --     playNote 3.5 0.5 volume ('C', 3),

        --     playNote 4.0 0.5 volume ('E', 3),
        --     playNote 4.5 0.5 volume ('F', 3),
        --     playNote 5.0 0.5 volume ('G', 3)
        -- ]
    ]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
