module Main
  where

import Data.Map                        ((!))
import Language.Chords                 (Chord (Chord), ChordType (Major))
import Language.Instrument             (Amplitude, Instrument (Instrument),
                                        InstrumentEvent (ChordEvent, NoteEvent))
import Language.Modulators
import Language.MusicPiece
import Language.Notes
import Notes.Default
import Synthesizer.Converters.Language (convertMusicPieceToSynthesizer)
import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers           (applyAmplitude)
import Synthesizer.Modifiers.Envelopes
import Synthesizer.Oscillator
import Synthesizer.Structure

defaultVolume :: Amplitude
defaultVolume = 25000

volume = 0.5
defaultDuration = 0.3
longDuration = 1
offset = 0.3
pauze = 0.05

musicPiece :: MusicPiece
musicPiece = MusicPiece [

    Instrument 440 defaultVolume (noteStrike 0.3 0.5 0.5 4) [
      NoteEvent (noteOffset 0 0)   longDuration 0.2 (C ^= Three),
      NoteEvent (noteOffset 0 0)   longDuration 0.2 (C ^= Two),

      NoteEvent (noteOffset 12 4)  longDuration 0.2 (B ^= Three),
      NoteEvent (noteOffset 12 4)  longDuration 0.2 (B ^= Two),

      NoteEvent (noteOffset 24 8)  longDuration 0.2 (A ^= Three),
      NoteEvent (noteOffset 24 8)  longDuration 0.2 (A ^= Two),

      NoteEvent (noteOffset 28 8)  longDuration 0.2 (F ^= Two),
      NoteEvent (noteOffset 28 8)  longDuration 0.2 (F ^= One)
    ],

    Instrument 440 defaultVolume (noteStrike 0.05 0.1 1 0.05) [

      -- first four
      NoteEvent (noteOffset 0 0)   defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 1 0)   defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 2 0)   defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 3 1)   defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 4 1)   defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 5 1)   defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 6 2)   defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 7 2)   defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 8 2)   defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 9 3)   defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 10 3)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 11 3)  defaultDuration (volume * 0.7) (E ^= Four),

      -- second four
      NoteEvent (noteOffset 12 4)  defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 13 4)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 14 4)  defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 15 5)  defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 16 5)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 17 5)  defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 18 6)  defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 19 6)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 20 6)  defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 21 7)  defaultDuration volume         (G ^= Three),
      NoteEvent (noteOffset 22 7)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 23 7)  defaultDuration (volume * 0.7) (E ^= Four),

      -- third four
      NoteEvent (noteOffset 24 8)  defaultDuration volume         (A ^= Three),
      NoteEvent (noteOffset 25 8)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 26 8)  defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 27 9)  defaultDuration volume         (A ^= Three),
      NoteEvent (noteOffset 28 9)  defaultDuration (volume * 0.8) (C ^= Four),
      NoteEvent (noteOffset 29 9)  defaultDuration (volume * 0.7) (E ^= Four),

      NoteEvent (noteOffset 30 10)  defaultDuration volume         (A ^= Three),
      NoteEvent (noteOffset 31 10)  defaultDuration (volume * 0.8) (D ^= Four),
      NoteEvent (noteOffset 32 10)  defaultDuration (volume * 0.7) (F ^= Four),

      NoteEvent (noteOffset 33 11)  defaultDuration volume         (A ^= Three),
      NoteEvent (noteOffset 34 11)  defaultDuration (volume * 0.8) (D ^= Four),
      NoteEvent (noteOffset 35 11)  defaultDuration (volume * 0.7) (F ^= Four)

    ]
  ]
    where
      chordNote  = C ^= Four
      noteOffset offsetAmount pauzeAmount = offset * offsetAmount + pauze * pauzeAmount


mediumPrioritySignal :: SynSound
mediumPrioritySignal = convertMusicPieceToSynthesizer musicPiece

main :: IO ()
main = do
    saveSignal "moonlight-sonata" mediumPrioritySignal
