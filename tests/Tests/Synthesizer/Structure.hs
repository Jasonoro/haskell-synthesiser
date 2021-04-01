module Tests.Synthesizer.Structure where

import Test.Tasty.HUnit
import Synthesizer.Structure

synthesizerStructureTests =
  [ testSampleGenerationSimple
  , testSampleMultipleEvents
  , testSampleMultipleChannels
  , testSampleGenerationTimeSteps
  , testGetAllEvents
  , testGetAllEventsDuring
  , testAddChannel
  , testAddToNewChannel ]

testSampleGenerationSimple = testCase "a simple waveform is sampled correctly" $ soundToSamples structure 10 @?= replicate 20 100 ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline]
        timeline = [ SoundEvent 0 2 (const [100, 100..]) ]

testSampleMultipleEvents = testCase "a simple waveform with multiple events is sampled correctly"
  $ soundToSamples structure 10 @?= replicate 10 100 ++ replicate 10 200 ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline]
        timeline = [SoundEvent 0 2 (const [100, 100..]), SoundEvent 1 1 (const [100, 100..])]

testSampleMultipleChannels = testCase "a simple waveform with multiple channels is sampled correctly"
  $ soundToSamples structure 10 @?= replicate 10 100 ++ replicate 10 200 ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline1, Channel timeline2]
        timeline1 = [SoundEvent 0 2 (const [100, 100..])]
        timeline2 = [SoundEvent 1 1 (const [100, 100..])]

testSampleGenerationTimeSteps = testCase "a simple testcase with blank periods"
  $ soundToSamples structure 10 @?= [10..19] ++ replicate 10 0 ++ [20..29] ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline]
        timeline = [SoundEvent 0 1 (const [10..20]), SoundEvent 2 1 (const [20..30])]

testGetAllEvents = testCase "check if we get all events"
  $ getAllEvents structure @?= events
  where events = [SoundEvent 0 1 (const [10..20]), SoundEvent 2 1 (const [20..30]), SoundEvent 3 1 (const [30..40])]
        channels = [Channel (take 2 events), Channel [events !! 2]]
        structure = SynSound channels

testGetAllEventsDuring = testCase "check if we get all events during a specific period"
  $ getAllEventsDuring structure (2, 4) @?= eventsDuring
  where eventsDuring = [SoundEvent 1 2 (const [1..]), SoundEvent 2 2 (const [1..]), SoundEvent 3 2 (const [1..])]
        eventsNotDuring = [SoundEvent 0 2 (const [1..]), SoundEvent 4 1 (const [1..])]
        events = eventsDuring ++ eventsNotDuring
        channels = [Channel events]
        structure = SynSound channels

testAddChannel = testCase "check if we can add a channel"
  $ channels (addChannel structure channel) @?= [channel]
  where structure = SynSound []
        channel = Channel [SoundEvent 0 1 (const [10..20])]

testAddToNewChannel = testCase "check if we can add a new channel with events"
  $ channels (addToNewChannel structure events) @?= [Channel events]
  where structure = SynSound []
        events = [SoundEvent 0 1 (const [10..20])]
