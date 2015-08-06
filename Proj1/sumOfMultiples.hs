import Data.List (elem)


sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples xs max = foldl mySum 0 (accumulateAllMultiples xs (max-1))

mySum :: Int -> Int -> Int
mySum a b = a + b

accumulateAllMultiples :: [Int] -> Int -> [Int]
accumulateAllMultiples (x:[]) max = multiplesOfIn x [1..max] 
accumulateAllMultiples (x:xs) max = (foldl
    (\acc next -> 
        if (next `elem` acc)
            then 
                acc
            else
                acc ++ [next])
    (multiplesOfIn x [1..max])
    (accumulateAllMultiples xs max))


multiplesOfIn :: Int -> [Int] -> [Int]
multiplesOfIn base [] = []
multiplesOfIn base (x:xs)
        | baseIsMultipleOfX == True  = [x] ++ (multiplesOfIn base xs)
        | otherwise                  = (multiplesOfIn base xs)
    where baseIsMultipleOfX = isMultipleOf x base

isMultipleOf :: Int -> Int -> Bool
isMultipleOf 0 b = True 
isMultipleOf a b = if (a < 0) then False else (isMultipleOf (a-b) (b))


main = do
    let x = sumOfMultiples [3, 5] 1000
    putStrLn (show x)
    return x
