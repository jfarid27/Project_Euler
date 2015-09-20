prodSubStr :: Int -> Int
prodSubStrRec :: Str -> Int
prodSubStrRec digits numStr currentMax =
    let (stringOfNum) = (show numStr) in
        if (stringLengthEqualsDigits)
            then if (product numStr > currentMax)
                then numStr
                else max
            else if (firstDigits > currentMax)
                then (prodSubStrRec digits (tail numStr) firstDigits) 
                else (prodSubStrRec digits (tail numStr) currentMax)
    where
        stringLengthEqualsDigits = (length stringOfNum) == digits
        firstDigits = read (take digits (stringOfNum)) :: Int
