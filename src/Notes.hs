module Notes
  where

import Data.Map (Map, fromList, (!))

-- | 9 * length notes
numberOfScaleNotes :: Int
numberOfScaleNotes = 9 * length notes

-- | ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
notes :: [String]
notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- | Generates a Map of notes and their frequency given a base frequency for note A4
generateNotes :: Double            -- ^ Base frequency for note A4 (Most used value: 440 Hz)
              -> Map String Double -- ^ Returns list of notes with their frequency
generateNotes freqA4 = fromList [generateNote c0 i | i <- [0..numberOfScaleNotes-1]]
  where
    c0 :: Double
    c0 = freqA4 * (2 ** (-4.75))

-- | Generates a note and their frequency given the frequency for the note C0 and the offset from C0
generateNote :: Double -- ^ The frequency of note C0
             -> Int    -- ^ The offset from the note C0
             -> (String, Double) -- ^ A tuple with the note and their frequency
generateNote freqBase i = (note ++ show octave, freq)
  where
    freq :: Double
    freq = frequencySteps freqBase i
    note :: String
    note = notes !! (i `mod` length notes)
    semitone :: Int
    semitone = numberOfSemitones freqBase freq
    octave :: Int
    octave = semitone `div` length notes

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
