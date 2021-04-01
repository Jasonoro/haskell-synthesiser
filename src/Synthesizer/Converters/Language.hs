module Synthesizer.Converters.Language
  ( convertMusicPieceToSynthesizer
  ) where

import Data.Map                        (Map, (!))
import Language                        (Amplitude, AmplitudeMult, BaseAmplitude,
                                        Instrument (..), MusicPiece (..), Note,
                                        NoteEvent (NoteEvent))
import Notes                           (generateNotes)
import Synthesizer.Modifiers           (applyAmplitude)
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
convertInstrumentToChannel instrument = Channel $ map (applyEnvelope noteStrike . convertNoteEventsToSoundEvents noteMap baseAmplitude) noteEvents
  where
    (Instrument baseFreq baseAmplitude noteStrike noteEvents) = instrument
    noteMap = generateNotes baseFreq

convertNoteEventsToSoundEvents :: Map Note Frequency -> BaseAmplitude -> NoteEvent -> SoundEvent
convertNoteEventsToSoundEvents noteMap baseAmplitude noteEvent = SoundEvent startTime duration samples
  where
    (NoteEvent startTime duration amplitudeMult note) = noteEvent
    samples :: SamplingRate -> [Sample]
    samples samplingRate = convertFrequencyToSamples noteAmplitude samplingRate noteFreq
    noteFreq :: Frequency
    noteFreq = convertNoteToFrequency noteMap note
    noteAmplitude :: Amplitude
    noteAmplitude = baseAmplitude * amplitudeMult

convertNoteToFrequency :: Map Note Frequency -> Note -> Frequency
convertNoteToFrequency noteMap note = noteMap ! note

convertFrequencyToSamples :: Amplitude -> SamplingRate -> Frequency -> [Sample]
convertFrequencyToSamples amplitude samplingRate freq = (applyAmplitude amplitude . sineOscillator freq) samplingRate
