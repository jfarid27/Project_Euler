module FindSubStr
( prodSubStr
, prodSubStrRec
, productify
, takeHead
) where

prodSubStr :: Int -> String -> String
prodSubStr digits x = prodSubStrRec digits x 0

productify :: Int -> Int
productify xs = foldl (\acc next -> acc * next) 1
    (map (\x -> (read [x] :: Int)) (show xs) )

takeHead :: Int -> String -> Int
takeHead digits xs = read (take digits (xs))

prodSubStrRec :: Int -> String -> Int -> String
prodSubStrRec digits numStr currentMax =
    let stringOfNum = numStr
        stringLengthEqualsDigits = (length stringOfNum) == digits :: Bool
        firstDigits = takeHead digits stringOfNum :: Int
        firstDigitProduct =  productify (takeHead digits stringOfNum) :: Int
        currentMaxProduct = productify currentMax
    in
        if (stringLengthEqualsDigits)
        then if (firstDigitProduct > currentMaxProduct)
            then numStr
            else show currentMax
        else if (firstDigitProduct > currentMaxProduct)
            then (prodSubStrRec digits (tail stringOfNum) firstDigits)
            else (prodSubStrRec digits (tail stringOfNum) currentMax)
