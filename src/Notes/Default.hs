module Notes.Default where

import Data.Char
import Data.Map                        (Map, (!))
import Debug.Trace
import Notes                           (generateNotes)
import Synthesizer.Modifiers
import Synthesizer.Modifiers.Envelopes
import Synthesizer.Oscillator
import Synthesizer.Structure

-- Notes

notes440 :: Map String Double
notes440 = generateNotes 440

a5 :: Double
a5 = notes440 ! "A5"

b5 :: Double
b5 = notes440 ! "B5"

c5 :: Double
c5 = notes440 ! "C5"

d5 :: Double
d5 = notes440 ! "D5"

e5 :: Double
e5 = notes440 ! "E5"

f5 :: Double
f5 = notes440 ! "F5"

g5 :: Double
g5 = notes440 ! "G5"



-- Functions

type Octave = Int
type Amplitude = Double
type Note = Char

playNote :: Time -> Length -> Amplitude -> (Note, Octave) -> SoundEvent
playNote startTime noteDuration amplitudeAmount note =
    applyEnvelope envelope $
    SoundEvent startTime noteDuration (amplitude amplitudeAmount . soundWave)
    where
        soundWave = sineOscillator (notes440 ! (fst note : (show . snd $ note)))
        -- for a standard note, make the envelope dependant on the duration of the note
        envelope = Envelope (noteDuration * 0.4) (noteDuration * 0.6) 0.5 (noteDuration * 0.2)
