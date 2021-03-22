{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language
  where

import Prelude               hiding ((^))
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

getNoteFrequency :: Note -> Frequency
getNoteFrequency note = undefined

getChordNotes :: Chord -> [Note]
getChordNotes chord = undefined

getOctaveFromInt :: Int -> Octave
getOctaveFromInt 1 = One
getOctaveFromInt 2 = Two
getOctaveFromInt 3 = Three
getOctaveFromInt 4 = Four
getOctaveFromInt 5 = Five
getOctaveFromInt 6 = Six
getOctaveFromInt 7 = Seven
getOctaveFromInt 8 = Eight
getOctaveFromInt _ = error "Invalid Int for Octave"

getIntFromOctave :: Octave -> Int
getIntFromOctave One   = 1
getIntFromOctave Two   = 2
getIntFromOctave Three = 3
getIntFromOctave Four  = 4
getIntFromOctave Five  = 5
getIntFromOctave Six   = 6
getIntFromOctave Seven = 7
getIntFromOctave Eight = 8

musicPiece :: MusicPiece
musicPiece = MusicPiece [
    -- lead
    Instrument 440 [
      NoteEvent 0 1 note,
      NoteEvent 1 1 note
    ],
    -- bass
    Instrument 120 [
      NoteEvent 0 2 note
    ]
  ]
  where
    note = C ^= Three
