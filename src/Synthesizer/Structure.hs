module Synthesizer.Structure where

import Data.Int


newtype SynSound = SynSound {
  channels :: [Channel]
}

newtype Channel = Channel {
  timeline :: [SoundEvent]
}

type Time         = Double
type Length       = Int
type Sample       = Int
type Frequency    = Double
type SamplingRate = Int

data SoundEvent = SoundEvent {
  startTime :: Time,
  length    :: Length,
  samples   :: SamplingRate -> [Sample]
}
