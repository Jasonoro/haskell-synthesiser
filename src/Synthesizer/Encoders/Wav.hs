module Synthesizer.Encoders.Wav
  where

import qualified Codec.Audio.Wave        as W
import           Control.Exception       (catch, throwIO)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import           Data.Int                (Int16)
import           Prelude                 hiding (catch)
import           Synthesizer.Structure   (Sample, SynSound, soundToSamples)
import           System.Directory        (removeFile)
import           System.IO               (Handle)
import           System.IO.Error         (isDoesNotExistError)

-- | The sampling rate of the WAV file
samplingRate :: Num hz => hz
samplingRate = 44100

-- | Write the given given the callback data. Overwrites the file if it already exists.
writeWaveFile :: FilePath            -- ^ Where to save the file
              -> W.Wave              -- ^ Parameters of the WAVE file
              -> (Handle -> IO ())   -- ^ Callback that will be used to write WAVE data
              -> IO ()
writeWaveFile path wave writeData = do
  removeFile path `catch` ignoreDoesNotExists
  W.writeWaveFile path wave writeData
  where ignoreDoesNotExists e | isDoesNotExistError e = return ()
                              | otherwise = throwIO e

-- | Save the generated sound of the Synthesizer into a WAV file. Overwrites the file if it already exists.
saveSignal :: FilePath     -- ^ The file name or location. A .wav extension will be appended to the file
           -> SynSound     -- ^ The structure representing your sound
           -> IO ()
saveSignal filename sound = do
  let samples = soundToSamples sound samplingRate
  let numSamples = length samples
  let wave = W.Wave {
      W.waveFileFormat = W.WaveVanilla
      , W.waveSampleRate = samplingRate
      , W.waveSampleFormat = W.SampleFormatPcmInt 16
      , W.waveChannelMask = W.speakerMono
      , W.waveDataOffset = 0
      , W.waveDataSize = fromIntegral $ numSamples * 2
      , W.waveSamplesTotal = fromIntegral numSamples
      , W.waveOtherChunks = []
  }
  let wavfile = filename <> ".wav"
  writeWaveFile wavfile wave $ \handle -> B.hPutBuilder handle (mconcat $ B.int16LE <$> map sampleToI16 samples)

-- TODO: Don't we want to throw an error?
-- | Convert a sample to an Int16, as that is the biggest amplitude WAV supports
sampleToI16 :: Sample -> Int16
sampleToI16 s | s > 32767 = 32767
              | s < -32768 = -32768
              | otherwise = fromIntegral $ round s
