module Synthesizer.Converters.Language
  where

import Data.Map                        (Map, (!))
import Language.Instrument
import Language.MusicPiece
import Language.Notes
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
convertInstrumentToChannel instrument = Channel $ map (convertInstrumentEventsToSoundEvents noteMap) instrumentEvents
  where
    (Instrument baseFreq instrumentEvents) = instrument
    noteMap = generateNotes baseFreq

convertInstrumentEventsToSoundEvents :: Map Note Frequency -> InstrumentEvent -> SoundEvent
convertInstrumentEventsToSoundEvents noteMap noteEvent = applyEnvelope amplitudeEnvelope $ SoundEvent startTime duration samples
  where
    (NoteEvent startTime duration note) = noteEvent
    samples :: SamplingRate -> [Sample]
    samples samplingRate = convertFrequencyToSamples samplingRate noteFreq
    noteFreq :: Frequency
    noteFreq = convertNoteToFrequency noteMap note
    amplitudeEnvelope = Envelope 0.2 0.3 0.5 0.2

convertNoteToFrequency :: Map Note Frequency -> Note -> Frequency
convertNoteToFrequency noteMap note = noteMap ! note

convertFrequencyToSamples :: SamplingRate -> Frequency -> [Sample]
convertFrequencyToSamples samplingRate freq = (amplitude amplitudeAmount . sineOscillator freq) samplingRate
  where
    amplitudeAmount = 32767
