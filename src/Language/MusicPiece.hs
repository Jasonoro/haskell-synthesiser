module Language.MusicPiece
  ( MusicPiece (..)
  ) where

import Language.Instrument (Instrument)

newtype MusicPiece = MusicPiece [Instrument]
  deriving (Show)
