module FindSubStr
( prodSubStr
) where

prodSubStr :: Int -> Int -> Int
prodSubStr digits x = prodSubStrRec digits x 0

prodSubStrRec :: Int -> Int -> Int -> Int
prodSubStrRec digits numStr currentMax =
    let stringOfNum = (show numStr)
        stringLengthEqualsDigits = (length stringOfNum) == digits
        firstDigits = read (take digits (stringOfNum)) :: Int
        firstDigitProduct = foldl (\acc next -> acc * next) 1
            (map (\x -> (read [x] :: Int)) (take digits (stringOfNum :: [Char])))
    in
        if (stringLengthEqualsDigits)
        then if (firstDigitProduct > currentMax)
            then numStr
            else currentMax
        else if (firstDigitProduct > currentMax)
            then (prodSubStrRec digits (read (tail stringOfNum) :: Int) firstDigits)
            else (prodSubStrRec digits (read (tail stringOfNum) :: Int) currentMax)
