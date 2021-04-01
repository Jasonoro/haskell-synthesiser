module Tests.Synthesizer.Modifiers.Envelopes
  where

import Synthesizer.Modifiers.Envelopes
import Synthesizer.Oscillator
import Synthesizer.Structure
import Test.Tasty.HUnit

envelopesTests = envelopes

soundEvent = SoundEvent 0 3 (const [1, 1..])

sr = 10
src = fromIntegral sr
makeReadable = map (round . (* 100))
getSamplesFromEnvelope :: Envelope -> SoundEvent -> [Sample]
getSamplesFromEnvelope env se = (samples $ applyEnvelope env se) sr

envelopes = [

  -- Attack Steps
  testCase "Attacksteps - in 1 second"
  $ makeReadable (getAttackSteps 1 src)          @?= [0,10,20,30,40,50,60,70,80,90,100],

  testCase "Attacksteps - in 10 second"
  $ makeReadable (getAttackSteps 10 src)         @?= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],

  testCase "Attacksteps - in 0 second"
  $ makeReadable (getAttackSteps 0 src)          @?= [],

  -- Decay Steps
  testCase "Decaysteps - in 1 second to 0.5 sustainlevel (to half volume)"
  $ makeReadable (getDecaySteps 1 0.5 src)       @?= [95,90,85,80,75,70,65,60,55,50],

  testCase "Decaysteps - in 1 second to 0.0 sustainlevel (to silent)"
  $ makeReadable (getDecaySteps 1 0 src)         @?= [90,80,70,60,50,40,30,20,10,0],

  testCase "Decaysteps - in 1 second to 1 sustainlevel (stay to same)"
  $ makeReadable (getDecaySteps 1 1 src)         @?= [100,100,100,100,100,100,100,100,100,100],

  testCase "Decaysteps - in 0 second to 0.5 sustainlevel (no decay steps needed)"
  $ makeReadable (getDecaySteps 0 0.5 src)       @?= [],

  -- Sustain steps
  testCase "Sustainsteps - 5 seconds total = 2 attack + 2 decay + 1 automatic sustain"
  $ makeReadable (getSustainSteps 0.5 5 2 2 src) @?= [50,50,50,50,50,50,50,50,50,50],

  testCase "Sustainsteps - 4 seconds total = 2 attack + 2 decay + 0 automatic sustain"
  $ makeReadable (getSustainSteps 0.5 4 2 2 src) @?= [],

  -- Release steps
  testCase "Releasesteps - 1 seconds extra from 0.5 sustainlevel to silent"
  $ makeReadable (getReleaseSteps 0.5 1 src)     @?= [45,40,35,30,25,20,15,10,5,0],

  testCase "An standard envelope - Envelope 1 1 0.5 1"
  $ makeReadable (getSamplesFromEnvelope (Envelope 1 1 0.5 1) soundEvent)        @?= [0,10,20,30,40,50,60,70,80,90,100,95,90,85,80,75,70,65,60,55,50,50,50,50,50,50,50,50,50,50,50,45,40,35,30,25,20,15,10,5,0],

  testCase "Almost 1 sustain - Envelope 1 1 0.99999 1"
  $ makeReadable (getSamplesFromEnvelope (Envelope 1 1 0.99999 1) soundEvent)    @?= [0,10,20,30,40,50,60,70,80,90,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,90,80,70,60,50,40,30,20,10,0],

  testCase "Almost 0 sustain - Envelope 1 1 0 1"
  $ makeReadable (getSamplesFromEnvelope (Envelope 1 1 0.00001 1) soundEvent)    @?= [0,10,20,30,40,50,60,70,80,90,100,90,80,70,60,50,40,30,20,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],

  testCase "A higher sustain - Envelope 1 1 2 1"
  $ makeReadable (getSamplesFromEnvelope (Envelope 1 1 2 1) soundEvent)          @?= [0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,200,200,200,200,200,200,200,200,200,200,180,160,140,120,100,80,60,40,20,0],

  testCase "No release - Envelope 1 1 0.5 0"
  $ makeReadable (getSamplesFromEnvelope (Envelope 1 1 0.5 0) soundEvent)        @?= [0,10,20,30,40,50,60,70,80,90,100,95,90,85,80,75,70,65,60,55,50,50,50,50,50,50,50,50,50,50,50]

  ]
