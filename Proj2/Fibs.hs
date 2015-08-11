import Data.Fixed

fibs :: Int -> Int
fibs 1 = 1
fibs 2 = 2
fibs x = (fibs (x - 1)) + (fibs (x - 2))

sumEvenFibsTail :: Int -> Int -> Int -> Int -> Int
sumEvenFibsTail max prev1 prev2 curr
    | (nextFib > max)    = curr
    | (isEven nextFib) = sumEvenFibsTail max nextFib prev1 (curr + nextFib)
    | otherwise        = sumEvenFibsTail max nextFib prev1 curr
    where
        nextFib = prev1 + prev2

sumEvenFibs :: Int -> Int
sumEvenFibs 0 = 0
sumEvenFibs 1 = 0
sumEvenFibs 2 = 2
sumEvenFibs maxSize = sumEvenFibsTail maxSize (fibs 2) (fibs 1) 2

isEven :: Int -> Bool
isEven 0 = False
isEven 1 = False
isEven x = ((rem x 2) == 0)

main = do
    let x = sumEvenFibs 4000000
    putStrLn (show x)
    return x
