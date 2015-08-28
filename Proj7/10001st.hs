import qualified Sieves as S

main = do
    x <- return ((!!) primes 10001)
    putStrLn (show x)
    return x 
        where primes = S.erasPrimes
