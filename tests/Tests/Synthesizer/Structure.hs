module Tests.Synthesizer.Structure where

import Test.Tasty.HUnit
import Synthesizer.Structure

synthesizerStructureTests =
  [ testSampleGenerationSimple
  , testSampleMultipleEvents
  , testSampleMultipleChannels
  , testSampleGenerationTimeSteps ]

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
