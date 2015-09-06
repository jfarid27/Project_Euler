import qualified Sieves as S

isFactor :: Int -> Int -> Bool
isFactor x y = (mod y x) == 0

primeFactors :: Int -> [Int]
primeFactors s = findFactors s [] S.erasPrimes

findFactors :: Int -> [Int] -> [Int] -> [Int]
findFactors x store [] = undefined -- No primes throws error
findFactors x store primesLeft
    |  (x == nextPrime)     = x : store -- If we get down to a prime
    |  nextPrimeIsFactor    = findFactors (quot x nextPrime) (nextPrime : store) (primesLeft)
    |  otherwise            = findFactors x (store) (tail primesLeft)
    where
        nextPrime = head primesLeft
        nextPrimeIsFactor = (nextPrime `isFactor` x)

main = do
    x <- return (primeFactors 600851475143)
    putStrLn (show (head x))
    return ()
