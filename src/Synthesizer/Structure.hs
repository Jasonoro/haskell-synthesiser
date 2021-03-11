module Synthesizer.Structure where

newtype SynSound = SynSound {
  channels :: [Channel]
}

newtype Channel = Channel {
  timeline :: [SoundEvent]
}

type Time         = Double
type Length       = Double
type Sample       = Int
type Frequency    = Double
type SamplingRate = Int

data SoundEvent = SoundEvent {
  startTime   :: Time,
  eventLength :: Length,
  samples     :: SamplingRate -> [Sample]
}

-- TODO: This isn't a very efficient implementation, but it seems to be fast enough for now
soundToSamples :: SynSound -> SamplingRate -> [Sample]
soundToSamples sound rate = soundToSamples' sound rate 0

soundToSamples' :: SynSound -> SamplingRate -> Int -> [Sample]
soundToSamples' sound rate sampleNumber | done = []
                                        | otherwise = sum samplesCurrentEvents : soundToSamples' sound rate (sampleNumber + 1)
  where (currentEvents, done) = getCurrentEvents sound (fromIntegral sampleNumber / fromIntegral rate)
        samplesCurrentEvents :: [Sample]
        samplesCurrentEvents = map (\e -> samples e rate !! sampleNumber) currentEvents


getCurrentEvents :: SynSound -> Time -> ([SoundEvent], Bool)
getCurrentEvents sound time | not (null currentEvents) = (currentEvents, False)
                            | otherwise                = (currentEvents, isDone)
  where flatEvents = concatMap timeline (channels sound)
        currentEvents = filter (\e -> time >= startTime e && time <= startTime e + eventLength e) flatEvents
        isDone = not (any (\e -> time < startTime e + eventLength e) flatEvents)
