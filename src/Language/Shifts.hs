{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Shifts
  ( (#)
  , (^)
  ) where

import Language.Notes        (Note (getTone), Octave, Tone, getIntFromOctave,
                              getIntFromTone, getOctave, getOctaveFromInt,
                              getToneFromInt)
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
  (#) note toneShift = note { getTone = newTone }
    where
      newTone = getToneFromInt $ getIntFromTone (getTone note) + toneShift

instance ShiftTone [Note] where
  (#) notes toneShift = map (# toneShift) notes
