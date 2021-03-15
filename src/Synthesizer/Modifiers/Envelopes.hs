module Synthesizer.Modifiers.Envelopes where

import Debug.Trace
import Synthesizer.Structure

type AttackLength  = Double
type DecayLength   = Double
type SustainLevel  = Double
type ReleaseLength = Double

data Envelope = Envelope {
    attackLength  :: AttackLength, -- ^ in seconds
    decayLength   :: DecayLength,  -- ^ in seconds
    sustainLevel  :: SustainLevel, -- ^ in seconds
    releaseLength :: ReleaseLength -- ^ in seconds
}


-- attackLenght : 5 seconden
-- samplingRate : 2000
-- attackSteppers : 10000 ->  [0, 1]

-- [2,2,2]
-- [1,2,3]
-- [2,4,6]

applyEnvelope :: Envelope -> SoundEvent -> SoundEvent
applyEnvelope envelope soundEvent = SoundEvent startTime newEventLength newSamples
    where
        SoundEvent startTime eventLength samples = soundEvent
        (Envelope attackLength decayLength sustainLevel releaseLength) = envelope
        newEventLength = eventLength + releaseLength
        newSamples :: SamplingRate -> [Sample]
        -- TODO: multiplicate rd time the last sample from the zipwith list
        newSamples samplingRate = zipWith (*) envelopeSteps input ++ rd
            where
                input = samples samplingRate
                envelopeSteps :: [Double]
                envelopeSteps = ad ++ dd ++ sd
                sr :: Double
                sr = fromIntegral samplingRate
                ad :: [Double] -- [0.0, ..., 1.0]
                ad = [0.0, (1.0 / (attackLength * sr))..1.0]
                dd :: [Double] -- [1.0, .., sustainLevel]
                dd = tail [1.0, 1.0 - ((1.0 - sustainLevel) / (decayLength * sr))..sustainLevel]
                sd :: [Double] --  [sustainLevel]
                sd = replicate (round (sustainLength * sr)) sustainLevel
                    where
                        sustainLength = eventLength - attackLength - decayLength
                -- TODO: the release stepper could start above the sustain level if attack and decay are longer than the eventLength
                rd :: [Double] -- [sustainLevel, ..., 0.0]
                rd = tail [sustainLevel, (sustainLevel - (1 / (releaseLength * sr)))..0.0]
