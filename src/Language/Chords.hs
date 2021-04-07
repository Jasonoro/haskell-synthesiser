{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Chords
  ( Chord (..)
  , ChordType (..)
  ) where

import Language.Notes (Note)

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
getChordNotes chord = undefined
