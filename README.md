# Haskell-Synthesizer: A user-friendly synthesizer in natrive haskell

Haskell-Synthesizer is a Haskell native synthesizer designed for simplicity.
It provides an easy-to-extend API for the synthesizer, and a high-level DSL for making music.

### Documentation
Here is the link to the Haddock documentation: [Haskell Synthesizer Documentation](https://jasonoro.github.io/haskell-synthesizer-docs)

## Installation instructions

1. Clone the repo
2. `cd` into the repo
3. `stack install haskell-synthesizer`

## Example

```haskell
module Main
  where

import Language
import Notes.Default
import Synthesizer.Converters.Language (convertMusicPieceToSynthesizer)
import Synthesizer.Encoders.Wav
import Synthesizer.Structure

defaultVolume :: Amplitude
defaultVolume = 25000

musicPiece :: MusicPiece
musicPiece = MusicPiece [
 -- Instrument BaseFrequency BaseAmplitude NoteStrike
    Instrument 440 defaultVolume (noteStrike 0.2 0.2 0.5 0.1) [
   -- NoteEvent StartTime Length AmplitudeMult Note
      NoteEvent 0   0.5 1 (C ^= Four),
      NoteEvent 0.5 0.5 1 (D ^= Four),
      NoteEvent 1   0.5 1 (E ^= Four),
      NoteEvent 1.5 0.5 1 (C ^= Four),

      NoteEvent 2   0.5 1 (C ^= Four),
      NoteEvent 2.5 0.5 1 (D ^= Four),
      NoteEvent 3   0.5 1 (E ^= Four),
      NoteEvent 3.5 0.5 1 (C ^= Four),

      NoteEvent 4   0.5 1.3 (E ^= Four),
      NoteEvent 4.5 0.5 1 (F ^= Four),
      NoteEvent 5   0.5 1.3 (G ^= Four)
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
