module Main where

import Data.Map                 ((!))
import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers    (amplitude, roundToSample)
import Synthesizer.Oscillator
import Synthesizer.Structure

import Notes.Default

mediumPrioritySignal :: SynSound
mediumPrioritySignal = SynSound [
        Channel [
            SoundEvent 0   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "C3")),
            SoundEvent 0.5   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "D3")),
            SoundEvent 1   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "E3")),
            SoundEvent 1.5   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "C3")),

            SoundEvent 2   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "C3")),
            SoundEvent 2.5   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "D3")),
            SoundEvent 3   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "E3")),
            SoundEvent 3.5   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "C3")),

            SoundEvent 4   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "E3")),
            SoundEvent 4.5   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "F3")),
            SoundEvent 5   0.5 (map (round . (* 32767)) . sineOscillator (notes440 ! "G3"))
        ]
    ]

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
