{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Modulators
  where

import Language.Notes (Note (Note, getOctave, getTone), Octave, Pitch (..),
                       Tone)

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

class ModulateTone a where
  (#=) :: a -> Tone -> a

instance ModulateTone Note where
  (#=) note tone = note { getTone = tone }

instance ModulateTone [Note] where
  (#=) notes tone = map (#= tone) notes
