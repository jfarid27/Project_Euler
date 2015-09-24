module PythagoreanTrips (
  isPythagorean
, Triple
) where

type Triple = (Int, Int, Int)

generatePairs :: Int -> Int -> [(Int, Int)]
generatePairs 0 b = [(0, b)]
generatePairs a b = (a, b):rest
    where rest = generatePairs (a-1) (b+1)

generateTuples :: Int -> Int -> [Triple]
generateTuples 0 b = map (\(x, y) -> (0, x, y)) pairs
    where pairs = generatePairs b 0

generateTuples a b = concat [current, rest]
    where rest = generateTuples (a-1) (b+1)
          current = map (\(x, y) -> (a, x, y)) pairs
          pairs = generatePairs b 0

isPythagorean :: Triple -> Bool
isPythagorean (a, b, c) = (a < b) && (b < c) && ( ((a^(2 :: Integer)) + (b^(2 :: Integer))) == (c^(2 :: Integer)) )
