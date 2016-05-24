module Regression
 ( simpleRegression ) where

import Data.List
import Data.Maybe
import Control.Monad
import Statistics

simpleRegression :: (Floating a) => [a] -> [a] -> (Maybe a, Maybe a)
simpleRegression x y  = (slope, intercept)
                  where
                    slope      = (/) <$> covariance x y <*> variance x
                    intercept  = subtract <$> mean y <*> ((*) <$> slope <*> mean x)
