module Main where

import Synthesizer.Encoders.Wav

import Notes.Default

-- 3 pulses: C5, G5, D5
-- ts = 250 ms, 250 ms
-- td = 250 ms
-- tb = 30 s
mediumPrioritySignal :: [Sample]
mediumPrioritySignal = cycle $ concat
    [ note c5 250
    , silence 250
    , note g5 250
    , silence 250
    , note d5 250
    , silence 30000
    ]

-- 10 pulses: C5, A5, D5, E5, F5, C5, A5, D5, E5, F5
-- ts = 0.1 s, 0.1 s, 0.3 s, 0.1 s, 1 s, 0.1 s, 0.1 s, 0.3 s, 0.1 s, 1 s
-- td = 0.2 s
-- tb = 5 s
highPrioritySignal :: [Sample]
highPrioritySignal = cycle $ concat
    [ note c5 200
    , silence 100
    , note a5 200
    , silence 100
    , note d5 200
    , silence 300
    , note e5 200
    , silence 100
    , note f5 200
    , silence 100
    , note c5 200
    , silence 100
    , note a5 200
    , silence 100
    , note d5 200
    , silence 300
    , note e5 200
    , silence 100
    , note f5 200
    , silence 5000
    ]

main :: IO ()
main = do
    saveSignal "medium" $ take 5000 mediumPrioritySignal
    saveSignal "high" $ take 5000 highPrioritySignal
