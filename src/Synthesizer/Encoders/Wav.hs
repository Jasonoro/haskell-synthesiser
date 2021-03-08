module Synthesizer.Encoders.Wav where

import qualified Codec.Audio.Wave        as W
import           Control.Monad.IO.Class
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import           Data.Int
import           System.Directory
import           System.IO

type Sample = Int16

amplitude :: Num a => a
amplitude = 32767

msToSamples :: Int -> Int
msToSamples ms = (ms * samplingRate) `div` 1000

note :: Double -> Int -> [Sample]
note hz ms = take num $ round . (amplitude *) . sin <$> ts
  where
    num = msToSamples ms
    dt = hz * 2 * pi / samplingRate
    ts = [0,dt..]

silence :: Int -> [Sample]
silence ms = replicate num 0
  where num = msToSamples ms

samplingRate :: Num hz => hz
samplingRate = 11025

writeWaveFile :: FilePath            -- ^ Where to save the file
              -> W.Wave              -- ^ Parameters of the WAVE file
              -> (Handle -> IO ())   -- ^ Callback that will be used to write WAVE data
              -> IO ()
writeWaveFile path wave writeData = do
  removeFile path
  W.writeWaveFile path wave writeData

saveSignal :: FilePath -> [Sample] -> IO ()
saveSignal filename samples = do
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
  writeWaveFile wavfile wave $ \handle -> B.hPutBuilder handle (mconcat $ B.int16LE <$> samples)
