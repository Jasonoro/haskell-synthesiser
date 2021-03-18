{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language
  where

import Synthesizer.Structure (Frequency)

data Tone = C | D | E | F | G | A | B
  deriving (Show)

data Pitch = Flat | Sharp
  deriving (Show)

data Octave = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Show)

data Note = Note
  { getTone   :: Tone
  , getPitch  :: Pitch
  , getOctave :: Octave
  }
  deriving (Show)

data ChordType =
  Major
  | Minor
  | Diminished
  | MajorSeventh
  | MinorSeventh
  | DominantSeventh
  | Suspended
  | Augmented
  | Extended
  deriving (Show)

type BaseNote = Note

data Chord = Chord
  { getType     :: ChordType
  , getBaseNote :: BaseNote
  }
  deriving (Show)

type StartTime = Double
type Duration = Double
data NoteEvent = NoteEvent StartTime Duration Note

type BaseFrequency = Frequency
data Instrument = Instrument BaseFrequency [NoteEvent]

newtype MusicPiece = MusicPiece [Instrument]

class ModulateOctave a b where
  (^) :: a -> Octave -> b

instance ModulateOctave Note Note where
  (^) note octave = undefined

instance ModulateOctave [Note] [Note] where
  (^) note octave = undefined

instance ModulateOctave Tone Note where
  (^) tone octave = undefined

instance ModulateOctave [Tone] [Note] where
  (^) tones octave = undefined

getNoteFrequency :: Note -> Frequency
getNoteFrequency note = undefined

getChordNotes :: Chord -> [Note]
getChordNotes chord = undefined
