# Haskell-Synthesizer: A user-friendly synthesizer in natrive haskell

Haskell-Synthesizer is a Haskell native synthesizer designed for simplicity.
It provides an easy-to-extend API for the synthesizer, and a high-level DSL for making music.

## Installation instructions

1. Clone the repo
2. `cd` into the repo
3. `stack install haskell-synthesizer`

## Example

```haskell
module Main
  where

import Synthesizer.Converters.Language
import Synthesizer.Encoders.Wav
import Synthesizer.Structure
import Language
import Notes.Default

musicPiece :: MusicPiece
musicPiece = MusicPiece [
 -- Instrument BaseFrequency
    Instrument 440 [
   -- NoteEvent startTime length Note
      NoteEvent 0   0.5 (C ^= Four),
      NoteEvent 0.5 0.5 (D ^= Four),
      NoteEvent 1   0.5 (E ^= Four),
      NoteEvent 1.5 0.5 (C ^= Four),

      NoteEvent 2   0.5 (C ^= Four),
      NoteEvent 2.5 0.5 (D ^= Four),
      NoteEvent 3   0.5 (E ^= Four),
      NoteEvent 3.5 0.5 (C ^= Four),

      NoteEvent 4   0.5 (E ^= Four),
      NoteEvent 4.5 0.5 (F ^= Four),
      NoteEvent 5   0.5 (G ^= Four)
    ]
  ]

-- Converts the DSL into the synthesizer structure.
-- If you want more control, you can use this structure to add more
-- waveforms, notes or custom sounds.
synthesizerStructure :: SynSound
synthesizerStructure = convertMusicPieceToSynthesizer musicPiece

-- Save the sound to a "WAV" file
main :: IO ()
main = do
    saveSignal "example" synthesizerStructure
```

This will save a "example.wav" file into the root directory. 