module Encoders.Wav where

import qualified Codec.Audio.Wave       as W
import           Control.Monad.IO.Class
import           System.Directory
import           System.IO

writeWaveFile :: MonadIO m
    => FilePath          -- ^ Where to save the file
    -> W.Wave              -- ^ Parameters of the WAVE file
    -> (Handle -> IO ()) -- ^ Callback that will be used to write WAVE data
    -> m ()

writeWaveFile path wave writeData = do
    {- HLINT ignore "Redundant pure" -}
    pure $ removeFile path
    W.writeWaveFile path wave writeData

