module Synthesizer.Converters.Language
  where

import Data.Map                        (Map, (!))
import Language                        (Instrument (..), MusicPiece (..), Note,
                                        NoteEvent (NoteEvent))
import Notes                           (generateNotes)
import Synthesizer.Modifiers           (amplitude)
import Synthesizer.Modifiers.Envelopes (Envelope (Envelope), applyEnvelope)
import Synthesizer.Oscillator          (sineOscillator)
import Synthesizer.Structure           (Channel (..), Frequency, Sample,
                                        SamplingRate, SoundEvent (..),
                                        SynSound (..))

convertMusicPieceToSynthesizer :: MusicPiece -> SynSound
convertMusicPieceToSynthesizer musicPiece = SynSound $ map convertInstrumentToChannel instruments
  where
    (MusicPiece instruments) = musicPiece

convertInstrumentToChannel :: Instrument -> Channel
convertInstrumentToChannel instrument = Channel $ map (applyEnvelope noteStrike . convertNoteEventsToSoundEvents noteMap) noteEvents
  where
    (Instrument baseFreq noteStrike noteEvents) = instrument
    noteMap = generateNotes baseFreq

convertNoteEventsToSoundEvents :: Map Note Frequency -> NoteEvent -> SoundEvent
convertNoteEventsToSoundEvents noteMap noteEvent = SoundEvent startTime duration samples
  where
    (NoteEvent startTime duration note) = noteEvent
    samples :: SamplingRate -> [Sample]
    samples samplingRate = convertFrequencyToSamples samplingRate noteFreq
    noteFreq :: Frequency
    noteFreq = convertNoteToFrequency noteMap note

convertNoteToFrequency :: Map Note Frequency -> Note -> Frequency
convertNoteToFrequency noteMap note = noteMap ! note

convertFrequencyToSamples :: SamplingRate -> Frequency -> [Sample]
convertFrequencyToSamples samplingRate freq = (amplitude amplitudeAmount . sineOscillator freq) samplingRate
  where
    amplitudeAmount = 32767
