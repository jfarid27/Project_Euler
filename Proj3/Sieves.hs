module Sieves
( erastosthenes
, erasPrimes
) where

erastosthenes :: [Int] -> [Int]
erastosthenes (x:xs) = [x] ++ (erastosthenes
    (filter
        (\y -> ( not ((rem y x) == 0)))
            xs))

erasPrimes :: [Int]
erasPrimes = [2] ++ erastosthenes [2..]
