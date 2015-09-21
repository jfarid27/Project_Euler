import FindSubStr as ProdSubStr

main = do
    x <- return 43625637737373734
    result <- return (ProdSubStr.prodSubStr 13 x)
    putStrLn (show result)
    return result
