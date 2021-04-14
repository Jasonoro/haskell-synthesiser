module Synthesizer.Structure
  ( Channel (..)
  , Frequency
  , Length
  , PhaseLength
  , Sample
  , SamplingRate
  , SoundEvent (..)
  , SynSound (..)
  , Time
  , addChannel
  , addToNewChannel
  , getAllEvents
  , getAllEventsDuring
  , soundToSamples
  ) where

import Data.Foldable (Foldable (foldr'))
import Data.List     (sortOn)

newtype SynSound = SynSound {
  channels :: [Channel]
} deriving (Show, Eq)

instance Semigroup SynSound where
  a <> b = foldl addChannel a (channels b)

instance Monoid SynSound where
  mempty = SynSound []

newtype Channel = Channel {
  timeline :: [SoundEvent]
} deriving (Show, Eq)

instance Semigroup Channel where
  a <> b = Channel (timeline a ++ timeline b)

instance Monoid Channel where
  mempty = Channel []

type Time         = Double
type Length       = Double
type Sample       = Double
type Frequency    = Double
type PhaseLength  = Int
type SamplingRate = Int

data SoundEvent = SoundEvent {
  startTime   :: Time,
  eventLength :: Length,
  samples     :: SamplingRate -> [Sample]
}

-- | Eq instance for SoundEvents. It should be noted that this isn't 100% sound, as if the needed samples for generating
-- | sounds are exactly the same but a sample after that is different, this will return equality even if it really
-- | isn't. However functionally these two events are the same, since at maximum that amount
-- | of samples will be used when generating the sound from them. So there is no practical difference in this context
instance Eq SoundEvent where
  a == b = startTime a == startTime b &&
           eventLength a == eventLength b &&
           takeNeededSamples a rate == takeNeededSamples b rate
    where rate = 100

instance Show SoundEvent where
  show (SoundEvent startTime eventLength _) = "SoundEvent { startTime = " ++ show startTime ++ " eventLength = " ++ show eventLength ++ " samples = ... }"

data SoundEventCached = SoundEventCached {
  event         :: SoundEvent,
  samplesCached :: [Sample]
}

-- | Takes the maximum needed samples to generate the sound. This is dependent on the sampling rate and the length of
-- | the event.
takeNeededSamples :: SoundEvent -> SamplingRate -> [Sample]
takeNeededSamples e rate = take (rate * ceiling (eventLength e) + 1) (samples e rate)

-- | Converts the sound structure to a list of samples with a certain sampling rate.
-- | The worst-case time complexity of the algorithm is @O(n log n)@, where n is the amount of sound events.
soundToSamples :: SynSound -> SamplingRate -> [Sample]
soundToSamples sound rate = soundToSamples' convertedEvents [] rate 0
  where
    sortedEvents = sortOn startTime (getAllEvents sound)
    convertedEvents = map eagerEvaluate sortedEvents
    eagerEvaluate e = SoundEventCached e (eagerSamples e)
    eagerSamples  e = takeNeededSamples e rate

soundToSamples' :: [SoundEventCached] -> [SoundEventCached] -> SamplingRate -> Int -> [Sample]
soundToSamples' []      []     _    _            = []
soundToSamples' samToDo samCur rate sampleNumber = samplesCurrentEvents : soundToSamples' updatedToDo updatedCurrent' rate (sampleNumber + 1)
  where currentTime = fromIntegral sampleNumber / fromIntegral rate
        (updatedToDo, updatedCurrent) = mergeTodoAndCurrent (samToDo, samCur) currentTime
        samplesCurrentEvents = foldr' (\e t -> t + head (samplesCached e)) 0 updatedCurrent
        updatedCurrent' = map (\e -> e {samplesCached = tail (samplesCached e)}) updatedCurrent


mergeTodoAndCurrent :: ([SoundEventCached], [SoundEventCached]) -> Time -> ([SoundEventCached], [SoundEventCached])
mergeTodoAndCurrent (todo, current) time = (newTodo, newCurrent)
  where eventsToCurrent []     = []
        eventsToCurrent (x:xs) = if time >= startTime (event x) then x:eventsToCurrent xs else []
        eventsFromTodo xs      = filter (\e -> time < startTime (event e) + eventLength (event e)) xs
        newTodo = drop (length $ eventsToCurrent todo) todo
        newCurrent = eventsFromTodo $ current ++ eventsToCurrent todo


-- Multiple helper functions for easy adding/removing events

-- | Adds a channel to a SynSound
addChannel :: SynSound -> Channel -> SynSound
addChannel s c = s {channels = c : channels s}

-- | Adds SoundEvents to a new channel.
addToNewChannel :: SynSound -> [SoundEvent] -> SynSound
addToNewChannel s xs = addChannel s (Channel xs)

-- | Gets all events currently in the synthesizer.
getAllEvents :: SynSound -> [SoundEvent]
getAllEvents s = concatMap timeline (channels s)

-- | Gets all events that overlap with a time period of (startTime, endTime).
-- | Passing an endTime that is before the startTime will result in no events being returned.
getAllEventsDuring :: SynSound -> (Time, Time) -> [SoundEvent]
getAllEventsDuring s (start, end) = filter (\e -> start < (startTime e + eventLength e) && startTime e < end) events
  where events = getAllEvents s
