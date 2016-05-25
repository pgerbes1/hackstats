module Utils
(roundAt) where

roundAt :: Int -> Double -> Double
roundAt d n = round(n * places) / places
            where places = d ^ 10


