module Language.Instrument
  ( Amplitude
  , BaseAmplitude
  , Instrument (..)
  , InstrumentEvent (..)
  ) where

import Language.Chords                 (Chord)
import Language.Notes                  (Note)
import Synthesizer.Modifiers.Envelopes (Envelope (Envelope))
import Synthesizer.Structure           (Frequency)

type StartTime = Double
type Duration = Double
type AmplitudeMult = Double
data InstrumentEvent = NoteEvent StartTime Duration AmplitudeMult Note | ChordEvent StartTime Duration AmplitudeMult Chord
  deriving (Show)

type BaseFrequency = Frequency
type Amplitude     = Double
type BaseAmplitude = Amplitude
type NoteStrike    = Envelope
data Instrument = Instrument BaseFrequency BaseAmplitude NoteStrike [InstrumentEvent]
  deriving (Show)
