import Control.Applicative

isPalindrome :: String -> Bool
isPalindrome "" = True
isPalindrome (x:[]) = True
isPalindrome (x:y:[]) = (x == y)
isPalindrome (x:xs) = if (x == (last (xs)))
    then (isPalindrome (init xs))
    else False

palindromesIn :: [Int] -> [Int]
palindromesIn xs = filter (\x -> isPalindrome (show x)) xs

myMaximum :: [Int] -> Int
myMaximum = foldl (\acc x -> (if x > acc then x else acc)) 0

main = do
    let palindromes = palindromesIn ( (*) <$> [0..999] <*> [0..999] ) 
    putStrLn (show (myMaximum palindromes))
