{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Shifts
  ( (#)
  , (^)
  ) where

import Debug.Trace           (trace)
import Language.Notes        (Note (getTone), Octave, Tone, getIntFromOctave,
                              getIntFromTone, getOctave, getOctaveFromInt,
                              getToneFromInt, tones)
import Prelude               hiding ((^))
import Synthesizer.Structure (Frequency)

type OctaveShift = Int
class ShiftOctave a where
  (^) :: a -> OctaveShift -> a

instance ShiftOctave Note where
  (^) note octaveShift = note { getOctave = newOctave }
    where
      newOctave = getOctaveFromInt $ getIntFromOctave (getOctave note) + octaveShift

instance ShiftOctave [Note] where
  (^) notes octaveShift = map (^ octaveShift) notes

type ToneShift = Int

class ShiftTone a where
  (#) :: a -> ToneShift -> a

instance ShiftTone Note where
  (#) note toneShift = note { getTone = getToneFromInt newTone } ^ octaveShift
    where
      previousTone = getIntFromTone (getTone note)
      tonesTotal = length tones
      tonesTillNextOctave = tonesTotal - previousTone

      octaveShift = (previousTone + toneShift) `div` tonesTotal
      newTone = (previousTone + toneShift) `mod` tonesTotal

instance ShiftTone [Note] where
  (#) notes toneShift = map (# toneShift) notes
