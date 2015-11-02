import qualified Sieves as S

lessThanMill :: Int
lessThanMill = sumWhileExclusive (\x -> x < 2000000) 0 S.erasPrimes

sumWhileExclusive :: (Int -> Bool) -> Int-> [Int] -> Int
sumWhileExclusive f store [] = store
sumWhileExclusive f store (x:xs)
	| not (f x)   = store
	| otherwise = sumWhileExclusive f (x + store) (xs)

main :: IO Int
main = do
	putStrLn (show lessThanMill)
	return lessThanMill
