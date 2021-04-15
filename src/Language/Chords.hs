{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Chords
  ( Chord (..)
  , ChordType (..)
  , getChordNotes
  ) where

import Language.Notes  (Note)
import Language.Shifts ((#))

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

getChordNotes :: Chord -> [Note]
getChordNotes (Chord Major baseNote) = [baseNote, baseNote # 2, baseNote # 4]
