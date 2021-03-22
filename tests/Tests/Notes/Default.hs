module Tests.Notes.Default where

import Test.Tasty.HUnit
import Notes.Default

defaultNotesTests =
  [ testA5Freq
  , testB5Freq
  , testC5Freq
  , testD5Freq
  , testE5Freq
  , testF5Freq
  , testG5Freq ]

testA5Freq = testCase "The correct frequency for a A5 note is generated" $ a5 @?= 879.9999999999899
testB5Freq = testCase "The correct frequency for a B5 note is generated" $ b5 @?= 987.7666025122364
testC5Freq = testCase "The correct frequency for a C5 note is generated" $ c5 @?= 523.2511306011921
testD5Freq = testCase "The correct frequency for a D5 note is generated" $ d5 @?= 587.3295358348091
testE5Freq = testCase "The correct frequency for a E5 note is generated" $ e5 @?= 659.2551138257329
testF5Freq = testCase "The correct frequency for a F5 note is generated" $ f5 @?= 698.4564628660003
testG5Freq = testCase "The correct frequency for a G5 note is generated" $ g5 @?= 783.9908719634899
