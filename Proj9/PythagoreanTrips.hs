type Triple = (Int, Int, Int)

isPythagorean :: Triple -> Bool
isPythagorean (a, b, c) = (a < b) && (b < c) && ( ((a^(2 :: Integer)) + (b^(2 :: Integer))) == (c^(2 :: Integer)) )

fan :: Triple -> [Triple]
fan (x,y,z) = [(x + 1, y, z), (x, y + 1, z), (x, y, z + 1)]

convolve :: Int -> [Triple]
convolve 1 = [(0, 0, 1), (0, 1, 0), (1, 0, 0)]
convolve x = do
	prev <- convolve (x - 1)
	updated <- fan prev
	return updated

filt :: [Triple] -> IO [Triple]
filt y = return (filter (\x -> isPythagorean x) (y))

main :: IO [Triple]
main = do
	x <- (filt (convolve 1000))
	putStrLn (show x)
	return x
