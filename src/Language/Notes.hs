{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Notes
  ( Note (..)
  , Octave (..)
  , Pitch (..)
  , Tone (..)
  , getIntFromOctave
  , getIntFromTone
  , getNoteFrequency
  , getOctaveFromInt
  , getToneFromInt
  , noteStrike
  , octaves
  , pitches
  , tones
  ) where

import Prelude                         hiding ((^))
import Synthesizer.Modifiers.Envelopes (AttackLength, DecayLength,
                                        Envelope (Envelope), ReleaseLength,
                                        SustainLevel)
import Synthesizer.Structure           (Frequency)

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

noteStrike :: AttackLength -> DecayLength -> SustainLevel -> ReleaseLength -> Envelope
noteStrike = Envelope

getOctaveFromInt :: Int -> Octave
getOctaveFromInt = toEnum

getIntFromOctave :: Octave -> Int
getIntFromOctave = fromEnum

getToneFromInt :: Int -> Tone
getToneFromInt = toEnum

getIntFromTone :: Tone -> Int
getIntFromTone = fromEnum
