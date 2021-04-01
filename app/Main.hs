module Main
  where

import Data.Map                        ((!))
import Synthesizer.Encoders.Wav
import Synthesizer.Modifiers           (amplitude)
import Synthesizer.Modifiers.Envelopes
import Synthesizer.Oscillator
import Synthesizer.Structure

import Language.Instrument
import Language.Modulators
import Language.MusicPiece
import Language.Notes
import Notes.Default
import Synthesizer.Converters.Language (convertMusicPieceToSynthesizer)

musicPiece :: MusicPiece
musicPiece = MusicPiece [
    Instrument 440 [
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


mediumPrioritySignal :: SynSound
mediumPrioritySignal = convertMusicPieceToSynthesizer musicPiece

main :: IO ()
main = do
    saveSignal "medium" mediumPrioritySignal
