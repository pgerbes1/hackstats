# hackstats
Basic Haskell statistics library built using Stack. 

Simple linear regression example. Pulls data from file and prints 
coefficients. 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bifunctor
import Data.Csv
import Data.Maybe
import qualified Data.Vector as V
import Regression
import Text.Printf

data Feature = Feature
           { xlab :: !Double
           , ylab :: !Double } deriving (Show, Read)

instance FromNamedRecord Feature where
    parseNamedRecord r = Feature <$> r .: "xlab" <*> r .: "ylab"

valuesToList :: Feature -> (Double, Double)
valuesToList (Feature c1 c2) = (c1, c2)

main :: IO ()
main = do
       csvData <- BL.readFile "data/simpleData.csv"
       case decodeByName csvData of
           Left err -> putStrLn err
           Right (_, v) -> putStrLn $ show "{Results: " ++ (show output) ++ " }"
              where tups   =  unzip . V.toList $ V.map valuesToList v
                    (x, y) = (fst tups, snd tups)
                    reg    = simpleRegression x y
                    (slope, intercept)   = join bimap (fromJust) reg
                    output = "[ Intercept: " ++ (show intercept) ++ " , Slope: " ++ (show slope) ++ " ]"
```

Output: 

```
Running hackstats-exe...
"{Results: ""[ Intercept: -2.000000000000231 , Slope: 0.4999999999999985 ]" }
```

