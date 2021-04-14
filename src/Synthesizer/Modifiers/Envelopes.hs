module Synthesizer.Modifiers.Envelopes
  where

import Synthesizer.Structure (Length, Sample, SamplingRate,
                              SoundEvent (SoundEvent))

type Step          = Double
type AttackLength  = Double
type DecayLength   = Double
type SustainLevel  = Double
type ReleaseLength = Double
type TotalLength   = Length

data Envelope = Envelope {
    attackLength  :: AttackLength, -- ^ The time it takes a note to rise to its highest volume (in seconds)
    decayLength   :: DecayLength,  -- ^ The time it takes a note to falls to the sustain level (in seconds)
    sustainLevel  :: SustainLevel, -- ^ The percentage of the maximum volume which the decay length will fall to (between 0 and 1)
    releaseLength :: ReleaseLength -- ^ The time it takes a note to completely fall from the sustain level to silent (in seconds)
} deriving (Show)

-- | Apply an envelope to a SoundEvent. This creates a new SoundEvent with changed properties
applyEnvelope :: Envelope -> SoundEvent -> SoundEvent
applyEnvelope envelope soundEvent = SoundEvent startTime newEventLength newSamples
    where
        SoundEvent startTime eventLength samples = soundEvent
        (Envelope attackLength decayLength sustainLevel releaseLength) = envelope
        newEventLength = eventLength + releaseLength
        newSamples :: SamplingRate -> [Sample]
        newSamples samplingRate = appliedSamples ++ releaseSamples
            where
                -- Samples
                appliedSamples = zipWith (*) envelopeSteps input
                input = samples samplingRate

                -- ReleaseSamples - release needs the sound after the original samples
                releaseSamples =  zipWith (*) releaseSteps releaseInput
                releaseInput = drop (length envelopeSteps) input
                releaseSteps = rd

                -- The envelope steps
                envelopeSteps :: [Double]
                envelopeSteps = ad ++ dd ++ sd

                sr :: Double
                sr = fromIntegral samplingRate

                -- Calculate the attack steps based on attack length
                ad :: [Step] -- [0.0, ..., 1.0]
                ad = getAttackSteps attackLength sr

                -- Calculate the decay steps based on decay length
                dd :: [Step] -- [1.0, .., sustainLevel]
                dd = getDecaySteps decayLength sustainLevel sr

                -- Calculate the sustain steps based on sustain level
                sd :: [Step] --  [sustainLevel]
                sd = getSustainSteps sustainLevel eventLength attackLength decayLength sr

                -- TODO: the release stepper could start above the sustain level if attack and decay are longer than the eventLength
                -- Calculate the release steps based on release length
                rd :: [Step] -- [sustainLevel, ..., 0.0]
                rd = getReleaseSteps sustainLevel releaseLength sr


type SamplingRateConverted = Double

-- | Calculate the attack steps based on attack length
getAttackSteps :: AttackLength -> SamplingRateConverted -> [Step]
getAttackSteps attackLength samplingRate = [0.0, step .. 1.0]
  where
    step = 1.0 / (attackLength * samplingRate)

-- | Calculate the decay steps based on decay length
getDecaySteps :: DecayLength -> SustainLevel -> SamplingRateConverted -> [Step]
getDecaySteps 0 sustainLevel samplingRate = []
-- special case where sustain level would never decrease and steps would be infinite
getDecaySteps decayLength 1 samplingRate = replicate (round (decayLength * samplingRate)) 1
getDecaySteps decayLength sustainLevel samplingRate = tail [1.0, (1.0 - step) .. sustainLevel]
  where
    step = (1.0 - sustainLevel) / (decayLength * samplingRate)

-- | Calculate the sustain steps based on sustain level
getSustainSteps :: SustainLevel -> TotalLength -> AttackLength -> DecayLength -> SamplingRateConverted -> [Step]
getSustainSteps sustainLevel eventLength attackLength decayLength samplingRate = replicate (round (sustainLength * samplingRate)) sustainLevel
  where
    sustainLength = eventLength - attackLength - decayLength

-- | Calculate the release steps based on the release length and the sustain level
getReleaseSteps :: SustainLevel -> ReleaseLength -> SamplingRateConverted -> [Step]
getReleaseSteps sustainLevel 0 samplingRate = []
getReleaseSteps sustainLevel releaseLength samplingRate  = tail [sustainLevel, (sustainLevel - step) .. 0.0]
  where
    step = 1 / (releaseLength * samplingRate) * sustainLevel
