module Synthesizer.Structure(
  SoundEvent(..),
  Channel(..),
  SynSound(..),
  Time,
  Length,
  Sample,
  Frequency,
  PhaseLength,
  SamplingRate,
  soundToSamples
) where

import Data.Foldable
import Data.List
import Data.Ord

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
type PhaseLength  = Int
type SamplingRate = Int

data SoundEvent = SoundEvent {
  startTime   :: Time,
  eventLength :: Length,
  samples     :: SamplingRate -> [Sample]
}

data SoundEventCached = SoundEventCached {
  event         :: SoundEvent,
  samplesCached :: [Sample]
}

soundToSamples :: SynSound -> SamplingRate -> [Sample]
soundToSamples sound rate = soundToSamples' convertedEvents [] rate 0
  where
    flatEvents = concatMap timeline (channels sound)
    sortedEvents = sortOn startTime flatEvents
    convertedEvents = map eagerEvaluate sortedEvents
    eagerEvaluate e = SoundEventCached e (eagerSamples e)
    eagerSamples  e = take (rate * ceiling (eventLength e) + 1) (samples e rate)

soundToSamples' :: [SoundEventCached] -> [SoundEventCached] -> SamplingRate -> Int -> [Sample]
soundToSamples' []      []     _    _            = []
soundToSamples' samToDo samCur rate sampleNumber = samplesCurrentEvents : soundToSamples' updatedToDo updatedCurrent rate (sampleNumber + 1)
  where currentTime = fromIntegral sampleNumber / fromIntegral rate
        (updatedToDo, updatedCurrent) = mergeTodoAndCurrent (samToDo, samCur) currentTime
        samplesCurrentEvents = foldr' (\e t -> t + samplesCached e !! (sampleNumber - sampleNumberStart (event e))) 0 updatedCurrent
        sampleNumberStart :: SoundEvent -> Int
        sampleNumberStart e = floor (startTime e) * rate


mergeTodoAndCurrent :: ([SoundEventCached], [SoundEventCached]) -> Time -> ([SoundEventCached], [SoundEventCached])
mergeTodoAndCurrent (todo, current) time = (newTodo, newCurrent)
  where eventsToCurrent []     = []
        eventsToCurrent (x:xs) = if time >= startTime (event x) then x:eventsToCurrent xs else []
        eventsFromTodo xs      = filter (\e -> time <= startTime (event e) + eventLength (event e)) xs
        newTodo = drop (length $ eventsToCurrent todo) todo
        newCurrent = eventsFromTodo $ current ++ eventsToCurrent todo
