module Tests.Synthesizer.Modifiers.Ceiling where

import Prelude hiding (ceiling, floor)
import Test.Tasty.HUnit
import Synthesizer.Structure
import Synthesizer.Modifiers.Ceiling

modifierCeilingTests =
  [ testCeilingModifier
  , testFloorModifier ]

testCeilingModifier = testCase "A ceiling can be put on a SoundEvent"
  $ take 6 (samples (ceiling 35 event) 1) @?= [10, 20, 30, 35, 35, 22]
  where event = SoundEvent 0 1 (const [10, 20, 30, 40, 50, 22])


testFloorModifier = testCase "A floor can be put on a SoundEvent"
  $ take 6 (samples (floor (-25) event) 1) @?= [0, -10, -20, -25, -25, -18]
  where event = SoundEvent 0 1 (const [0, -10, -20, -30, -40, -18])
