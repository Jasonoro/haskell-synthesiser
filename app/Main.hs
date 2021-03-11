module Main where

import Synthesizer.Encoders.Wav
import Synthesizer.Structure

import Notes.Default

mediumPrioritySignal :: SynSound
mediumPrioritySignal = SynSound [Channel [SoundEvent 0 4 (const $ concat $ repeat [1000, 0, -1000, 0])]]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
