import Data.Fixed

primes = [1..20] 
smallestMultiple :: () -> Int
smallestMultiple _ = findSmallestMultiple primes 1

findSmallestMultiple :: [Int] -> Int -> Int
findSmallestMultiple myInts current
    | isEvenlyDivisible = current
    | otherwise = findSmallestMultiple myInts (current + 1)
    where
        isEvenlyDivisible = (foldl and' True 
            (map (evenlyDivisible current) myInts))

and' :: Bool -> Bool -> Bool
and' x y = (x && y)

evenlyDivisible :: Int -> Int -> Bool
evenlyDivisible a b = (rem a b) == 0

main = do
    result <- return (smallestMultiple ())
    putStrLn (show result)
