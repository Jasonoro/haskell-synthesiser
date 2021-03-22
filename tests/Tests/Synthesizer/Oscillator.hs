module Tests.Synthesizer.Oscillator where

import Test.Tasty.HUnit
import Synthesizer.Oscillator

oscillatorStructureTests =
  [ testSawToothOscillator
  , testSineOscillator ]

testSawToothOscillator = testCase "The correct samples of a saw tooth osciallator are generated"
  $ map round (take 10 (sawToothOscillator 400 10)) @?= [0, 251, 103, 354, 205, 57, 308, 159, 11, 262]

testSineOscillator = testCase "Test correct samples of a sine oscillator are generated"
  $ map (\e -> round (e * 10**3)) (take 10 (sineOscillator 2 10)) @?= [0, 951, 588, -588, -951, 0, 951, 588, -588, -951]