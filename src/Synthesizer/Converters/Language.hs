module Synthesizer.Converters.Language
  where

import Language
import Notes                 (generateNotes)
import Synthesizer.Structure (Channel, Frequency, Sample, SoundEvent,
                              SynSound (..))

convertMusicPieceToSynthesizer :: MusicPiece -> SynSound
convertMusicPieceToSynthesizer musicPiece = SynSound $ map convertInstrumentToChannel instruments
  where
    (MusicPiece instruments) = musicPiece

convertInstrumentToChannel :: Instrument -> Channel
convertInstrumentToChannel instrument = undefined
  where
    (Instrument baseFreq noteEvents) = instrument
    noteMap = generateNotes baseFreq

convertNoteEventsToSoundEvents :: NoteEvent -> SoundEvent
convertNoteEventsToSoundEvents noteEvent = undefined
  where
    (NoteEvent startTime duration note) = noteEvent

convertNoteToFrequency :: Note -> Frequency
convertNoteToFrequency note = undefined
  where
    (Note tone pitch octave) = note

convertFrequencyToSamples :: Frequency -> [Sample]
convertFrequencyToSamples freq = undefined
