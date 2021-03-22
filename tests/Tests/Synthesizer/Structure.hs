module Tests.Synthesizer.Structure where

import Test.Tasty.HUnit
import Synthesizer.Structure

synthesizerStructureTests =
  [ testSampleGenerationSimple
  , testSampleMultipleEvents
  , testSampleMultipleChannels
  , testSampleGenerationTimeSteps ]

-- TODO: Seems like the sampling algorithm goes one too far, we should need a `replicate` 20
testSampleGenerationSimple = testCase "a simple waveform is sampled correctly" $ soundToSamples structure 10 @?= replicate 21 100 ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline]
        timeline = [ SoundEvent 0 2 (const [100, 100..]) ]

-- TODO: Seems like the sampling algorithm goes one too far, we should need a `replicate` 10
testSampleMultipleEvents = testCase "a simple waveform with multiple events is sampled correctly"
  $ soundToSamples structure 10 @?= replicate 10 100 ++ replicate 11 200 ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline]
        timeline = [SoundEvent 0 2 (const [100, 100..]), SoundEvent 1 1 (const [100, 100..])]

-- TODO: Seems like the sampling algorithm goes one too far, we should need a `replicate` 10
testSampleMultipleChannels = testCase "a simple waveform with multiple channels is sampled correctly"
  $ soundToSamples structure 10 @?= replicate 10 100 ++ replicate 11 200 ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline1, Channel timeline2]
        timeline1 = [SoundEvent 0 2 (const [100, 100..])]
        timeline2 = [SoundEvent 1 1 (const [100, 100..])]

-- TODO: Seems like the sampling algorithm goes one too far, we should need a `replicate` 10
testSampleGenerationTimeSteps = testCase "a simple testcase with blank periods"
  $ soundToSamples structure 10 @?= [10..20] ++ replicate 9 0 ++ [20..30] ++ [0]
  where structure = SynSound channels
        channels = [Channel timeline]
        timeline = [SoundEvent 0 1 (const [10..20]), SoundEvent 2 1 (const [20..30])]
