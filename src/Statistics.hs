module Statistics
 ( mean
 , variance
 , covariance
 , stdev
) where

import Data.List
import Data.Maybe
import Control.Monad

mean :: (Fractional a) => [a] -> Maybe a
mean []  = Nothing
mean [x] = Just x
mean  x  = Just ( sum x / genericLength x)

variance  :: (Fractional a) => [a] -> Maybe a
variance [x] =  Nothing
variance  x  =  (/n) <$> sigma
             where mu    = mean x
                   n     = genericLength x - 1
                   sigma = mu >>= \s -> return( sum $ (^2) . (subtract s) <$> x)

covariance   :: (Fractional a) => [a] -> [a] -> Maybe a
covariance _ []   =    Nothing
covariance [] _   =    Nothing
covariance x y
         | lx < 2        = Nothing
         | ly < 2        = Nothing
         | not(ly == lx) = Nothing
         | otherwise     = (/n) <$> sigma
                          where mux   = mean x
                                muy   = mean y
                                diffx = mux >>= \dx -> return( (subtract dx) <$> x)
                                diffy = muy >>= \dy -> return( (subtract dy) <$> y)
                                sigma = sum <$> liftM2 (zipWith (*)) diffx diffy
                                (n, lx, ly) = (genericLength x - 1, length x, length y)

stdev :: (Floating a) => [a] -> Maybe a
stdev x  = sqrt <$> (variance x)