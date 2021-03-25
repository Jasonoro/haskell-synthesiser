module Notes.Default
  where

import Data.Map (Map, (!))
import Language (Note, Octave (..), Tone (..), (^=))
import Notes    (generateNotes)

-- Notes

-- | Generate a note map at the base frequency of 440Hz. See 'generateNotes' for the more customisable implementation.
notes440 :: Map Note Double
notes440 = generateNotes 440

a5 :: Double
a5 = notes440 ! (A ^= Five)

b5 :: Double
b5 = notes440 ! (B ^= Five)

c5 :: Double
c5 = notes440 ! (C ^= Five)

d5 :: Double
d5 = notes440 ! (D ^= Five)

e5 :: Double
e5 = notes440 ! (E ^= Five)

f5 :: Double
f5 = notes440 ! (F ^= Five)

g5 :: Double
g5 = notes440 ! (G ^= Five)


-- Functions

