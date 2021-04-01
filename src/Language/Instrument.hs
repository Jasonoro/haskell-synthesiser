module Language.Instrument
  where

import Language.Chords       (Chord)
import Language.Notes        (Note)
import Synthesizer.Structure (Frequency)

type StartTime = Double
type Duration = Double
data InstrumentEvent = NoteEvent StartTime Duration Note | ChordEvent StartTime Duration Chord
  deriving (Show)

type BaseFrequency = Frequency
data Instrument = Instrument BaseFrequency [InstrumentEvent]
  deriving (Show)
