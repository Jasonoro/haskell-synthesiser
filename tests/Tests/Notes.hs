module Tests.Notes
  where

import Data.Map            ((!))
import Language.Modulators ((^=))
import Language.Notes      (Octave (..), Tone (..))
import Notes
import Test.Tasty.HUnit

notesTests =
  [ testNumberOfScaleNotes
  , test12Notes
  , testB5Note
  , testGenerateNote
  , testFrequencyStep
  , testNumberOfSemitones
  , testFrequencyToWaveLength
  , testWaveLengthToFrequency ]

testNumberOfScaleNotes    = testCase "the number of scale notes is 9 times the length of notes" $ amountOfNotes @?= 108
test12Notes               = testCase "there are 12 notes" $ length playableTonesPitches @?= 12
testB5Note                = testCase "the frequency of the B5 note is correct" $ generateNotes 440 ! (B ^= Five) @?= 987.7666025122364
testGenerateNote          = testCase "the generation of a D note is correct" $ generateNote 440 2 @?= (D ^= Zero, 493.88330125612396)
testFrequencyStep         = testCase "the correct frequency step is taken" $ frequencySteps 440 2 @?= 493.88330125612396
testNumberOfSemitones     = testCase "the correct number of semitones is generated" $ numberOfSemitones 440 500 @?= 2
testFrequencyToWaveLength = testCase "the correct frequency wavelength conversion is done" $ frequencyToWaveLength 440 @?= 0.7840909090909091
testWaveLengthToFrequency = testCase "the correct wavelength to frequency conversion is done" $ wavelengthToFrequency 0.5 @?= 690.0
