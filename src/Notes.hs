module Notes
  ( generateNotes
  ) where

import Data.Map              (Map, fromList, (!))
import Language              (Note (..), Pitch (..), Tone (..),
                              getOctaveFromInt, octaves)
import Synthesizer.Structure (Frequency)

-- | [C, C#, D, D#, E, F, F#, G, G#, A, A#, B]
playableTonesPitches :: [(Tone, Pitch)]
playableTonesPitches = [(C, Flat), (C, Sharp), (D, Flat), (D, Sharp), (E, Flat), (F, Flat), (F, Sharp), (G, Flat), (G, Sharp), (A, Flat), (A, Sharp), (B, Flat)]

-- | length octaves * length playableTonesPitches
amountOfNotes :: Int
amountOfNotes = length octaves * length playableTonesPitches

-- | Generates a Map of notes and their frequency given a base frequency for note A4
generateNotes :: Frequency          -- ^ Base frequency for note A4 (Most used value: 440 Hz)
              -> Map Note Frequency -- ^ Returns list of notes with their frequency
generateNotes freqA4 = fromList [generateNote c0 i | i <- [0..amountOfNotes-1]]
  where
    c0 :: Frequency
    c0 = freqA4 * (2 ** (-4.75))

-- | Generates a note and their frequency given the frequency for the note C0 and the offset from C0
generateNote :: Frequency         -- ^ The frequency of note C0
             -> Int               -- ^ The offset from the note C0
             -> (Note, Frequency) -- ^ A tuple with the note and their frequency
generateNote freqBase i = (Note tone pitch octave, freq)
  where
    (tone, pitch) = playableTonesPitches !! (i `mod` length playableTonesPitches)
    freq = frequencySteps freqBase i
    semitone = numberOfSemitones freqBase freq
    octave = getOctaveFromInt $ semitone `div` length playableTonesPitches

-- | The basic formula for the frequencies of the notes of the equal tempered scale.
--
--   Source: https://pages.mtu.edu/~suits/NoteFreqCalcs.html
frequencySteps :: Double -- ^ The number of half steps away from the fixed note
               -> Int    -- ^ The frequency of one fixed note
               -> Double -- ^ The frequency of the note n half steps away
frequencySteps freq n = freq * (a ^ n)
  where
    -- | a = (2)^(1/12)
    a :: Double
    a = 1.059463094359295

-- | Calculates the number n of semitones away from the base frequency
numberOfSemitones :: Double -- ^ The base frequency
                  -> Double -- ^ The frequency half steps away from the base frequency
                  -> Int    -- ^ The number of half steps away from the base frequency
numberOfSemitones freqBase freqN = round $ 12 * logBase 2 (freqN / freqBase)

-- | Converts the frequency to the wavelength in meters.
frequencyToWaveLength :: Double -- ^ The frequency in Hz
                      -> Double -- ^ The wavelength in meters
frequencyToWaveLength freq = c / freq
  where
    c :: Double -- ^ The speed of sound in m/s
    c = 345

-- | Converts the wavelength to the frequency in Hz.
wavelengthToFrequency :: Double -- ^ The wavelength in meters
                      -> Double -- ^ The frequency in Hz
wavelengthToFrequency wl = c / wl
  where
    c :: Double -- ^ The speed of sound in m/s
    c = 345
