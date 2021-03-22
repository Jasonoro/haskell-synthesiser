{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language
  where

import Prelude               hiding ((^))
import Synthesizer.Structure (Frequency)

data Tone = C | D | E | F | G | A | B
  deriving (Show, Enum, Ord, Eq)

tones :: [Tone]
tones = [C ..]

data Pitch = Flat | Sharp
  deriving (Show, Enum, Ord, Eq)

pitches :: [Pitch]
pitches = [Flat ..]

data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Show, Enum, Ord, Eq)

octaves :: [Octave]
octaves = [Zero ..]

data Note = Note
  { getTone   :: Tone
  , getPitch  :: Pitch
  , getOctave :: Octave
  }
  deriving (Show, Ord, Eq)

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
  deriving (Show, Enum)

type BaseNote = Note

data Chord = Chord
  { getType     :: ChordType
  , getBaseNote :: BaseNote
  }
  deriving (Show)

type StartTime = Double
type Duration = Double
data NoteEvent = NoteEvent StartTime Duration Note | ChordEvent StartTime Duration Chord
  deriving (Show)

type BaseFrequency = Frequency
data Instrument = Instrument BaseFrequency [NoteEvent]
  deriving (Show)

newtype MusicPiece = MusicPiece [Instrument]
  deriving (Show)

class ModulateOctave a b where
  (^=) :: a -> Octave -> b

instance ModulateOctave Note Note where
  (^=) note octave = note { getOctave = octave }

instance ModulateOctave [Note] [Note] where
  (^=) notes octave = map (^= octave) notes

instance ModulateOctave Tone Note where
  (^=) tone octave = Note tone Flat octave

instance ModulateOctave [Tone] [Note] where
  (^=) tones octave = map (^= octave) tones

type OctaveShift = Int

class ShiftOctave a b where
  (^) :: a -> OctaveShift -> b

instance ShiftOctave Note Note where
  (^) note octaveShift = note { getOctave = newOctave }
    where
      newOctave = getOctaveFromInt $ getIntFromOctave (getOctave note) + octaveShift

instance ShiftOctave [Note] [Note] where
  (^) notes octaveShift = map (^ octaveShift) notes

class ModulateTone a b where
  (#=) :: a -> Tone -> b

instance ModulateTone Note Note where
  (#=) note tone = note { getTone = tone }

instance ModulateTone [Note] [Note] where
  (#=) notes tone = map (#= tone) notes

type ToneShift = Int

class ShiftTone a b where
  (#) :: a -> ToneShift -> b

instance ShiftTone Note Note where
  (#) note toneShift = note { getTone = newTone }
    where
      newTone = getToneFromInt $ getIntFromTone (getTone note) + toneShift

instance ShiftTone [Note] [Note] where
  (#) notes toneShift = map (# toneShift) notes

getNoteFrequency :: Note -> Frequency
getNoteFrequency note = undefined

getChordNotes :: Chord -> [Note]
getChordNotes chord = undefined

getOctaveFromInt :: Int -> Octave
getOctaveFromInt = toEnum

getIntFromOctave :: Octave -> Int
getIntFromOctave = fromEnum

getToneFromInt :: Int -> Tone
getToneFromInt = toEnum

getIntFromTone :: Tone -> Int
getIntFromTone = fromEnum
