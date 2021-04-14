module Examples
  where

import Language.Instrument
import Language.Modulators
import Language.MusicPiece
import Language.Notes
import Notes.Default
import Synthesizer.Converters.Language (convertMusicPieceToSynthesizer)
import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers
import Synthesizer.Oscillator
import Synthesizer.Structure

defaultVolume :: Amplitude
defaultVolume = 25000

-- A piece of music written in the DSL. Multiple instrument can be combined into a single music piece
musicPiece :: MusicPiece
musicPiece = MusicPiece [
 -- Instrument BaseFrequency BaseAmplitude NoteStrike
    Instrument 440 defaultVolume (noteStrike 0.2 0.2 0.5 0.1) [
   -- NoteEvent StartTime Length AmplitudeMult Note
      NoteEvent 0   0.5 1 (C ^= Four),
      NoteEvent 0.5 0.5 1 (D ^= Four),
      NoteEvent 1   0.5 1 (E ^= Four),
      NoteEvent 1.5 0.5 1 (C ^= Four),

      NoteEvent 2   0.5 1 (C ^= Four),
      NoteEvent 2.5 0.5 1 (D ^= Four),
      NoteEvent 3   0.5 1 (E ^= Four),
      NoteEvent 3.5 0.5 1 (C ^= Four),

      NoteEvent 4   0.5 1.3 (E ^= Four),
      NoteEvent 4.5 0.5 1   (F ^= Four),
      NoteEvent 5   0.5 1.3 (G ^= Four)
    ]
  ]

-- Converts the DSL into the synthesizer structure.
-- If you want more control, you can use this structure to add more
-- waveforms, notes or custom sounds.
synthesizerStructure :: SynSound
synthesizerStructure = convertMusicPieceToSynthesizer musicPiece

-- Add a custom sound to the synthesizer
-- First: Create a channel. A channel can hold one or more events
channel = Channel [
    -- Second: Add a SoundEvent. The first argument is the start time, the second the time the event takes
    -- The last argument is the sound produced. Here, some default generators are used. Custom ones will be explained later
    SoundEvent 0 4 (applyAmplitude 1000 . sineOscillator 500)
  ]

-- Channels (and SynSounds) can be combined as they are an instance of Monoid.
doubleChannel = channel <> channel

-- Now we can add the channel to the synthesizer structure
-- If we now save this file, a new waveform will be inserted from t=0 to t=4 seconds
newStructure = addChannel synthesizerStructure channel

-- Writing custom generators
-- The basis of writing a custom sound generator is that it has to return some function of form (SamplingRate -> [Double])
-- So let's say we wish to write some function that does (+1) in a second.
gen :: SamplingRate -> [Sample]
gen rate = [0, timeStep..]
  where timeStep = 1 / fromIntegral rate
-- As you can see, this function returns an infinite list. While not strictly necessary, it is preferable.
-- The generator will work as long as it returns a list longer then (samplingRate * ceiling (eventLength e) + 1)


-- This generator can then be put into a SoundEvent. The SamplingRate will be passed into the function based on the file
-- you're creating
eventWithCustomGen = SoundEvent 0 10 (applyAmplitude 1000 . gen)
-- And add it to our synthesizer structure
newStructureWithCustomGen = addToNewChannel synthesizerStructure [eventWithCustomGen]


-- Save everything to WAV files
saveFiles :: IO ()
saveFiles = do
    saveSignal "dsl_only" synthesizerStructure
    saveSignal "with_added_sine" newStructure
    saveSignal "with_custom_gen" newStructureWithCustomGen
