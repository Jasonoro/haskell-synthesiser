module Synthesizer.Converters.Language
  ( convertMusicPieceToSynthesizer
  ) where

import Data.Map                        (Map, (!))
import Language.Instrument             (Amplitude, BaseAmplitude,
                                        Instrument (..),
                                        InstrumentEvent (NoteEvent))
import Language.MusicPiece
import Language.Notes
import Notes                           (generateNotes)
import Synthesizer.Modifiers           (applyAmplitude)
import Synthesizer.Modifiers.Envelopes (Envelope (Envelope), applyEnvelope)
import Synthesizer.Oscillator          (sineOscillator)
import Synthesizer.Structure           (Channel (..), Frequency, Sample,
                                        SamplingRate, SoundEvent (..),
                                        SynSound (..))


-- | Convert the DSL to a 'SynSound'. This allows the user to add custom sounds to the 'SynSound' if they so wish.
convertMusicPieceToSynthesizer :: MusicPiece -> SynSound
convertMusicPieceToSynthesizer musicPiece = SynSound $ map convertInstrumentToChannel instruments
  where
    (MusicPiece instruments) = musicPiece

-- | Converts an instrument block in the DSL to a Synthesizer channel.
convertInstrumentToChannel :: Instrument -> Channel
convertInstrumentToChannel instrument = Channel $ map (applyEnvelope noteStrike . convertNoteEventsToSoundEvents noteMap baseAmplitude) noteEvents
  where
    (Instrument baseFreq baseAmplitude noteStrike noteEvents) = instrument
    noteMap = generateNotes baseFreq

-- | Convert a single (possibly modified by envelope) note event to a SoundEvent.
convertNoteEventsToSoundEvents :: Map Note Frequency -> BaseAmplitude -> InstrumentEvent -> SoundEvent
convertNoteEventsToSoundEvents noteMap baseAmplitude noteEvent = SoundEvent startTime duration samples
  where
    (NoteEvent startTime duration amplitudeMult note) = noteEvent
    samples :: SamplingRate -> [Sample]
    samples samplingRate = convertFrequencyToSamples noteAmplitude samplingRate noteFreq
    noteFreq :: Frequency
    noteFreq = convertNoteToFrequency noteMap note
    noteAmplitude :: Amplitude
    noteAmplitude = baseAmplitude * amplitudeMult

-- | Converts a note to a frequency. First argument is the map of the base frequency of the instrument.
convertNoteToFrequency :: Map Note Frequency -> Note -> Frequency
convertNoteToFrequency noteMap note = noteMap ! note

convertFrequencyToSamples :: Amplitude -> SamplingRate -> Frequency -> [Sample]
convertFrequencyToSamples amplitude samplingRate freq = (applyAmplitude amplitude . sineOscillator freq) samplingRate
