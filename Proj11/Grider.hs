module Grider (
getBlocks
, getMaximum
, prod
) where 

import qualified Data.Map.Lazy as M

type Grid = [Int]
type Block = [Int]
type Blocks = [Block]
type Size = Int
data Direction = Up | UpLeft | Left | DownLeft | Down | UpRight | Right | DownRight
type IndexedGrid = M.Map Int Int

prod :: Block -> Int
prod ls = foldl (\agg x -> agg * x) 1 ls

getMaximum :: Blocks -> IO Block
getMaximum xs = return $ foldl
    (\agg next -> 
        if ((prod next) > (prod agg))
            then next 
            else agg ) [] xs

mapGrid :: Grid -> IndexedGrid 
mapGrid grid = M.fromList $ zip [0..] grid

blockIndexes :: Size -> Int -> Direction -> [Int]
blockIndexes n i Grider.Up = [i, i-n, i- (2*n), i - (3*n)]  
blockIndexes n i Grider.UpLeft = [i, i - n - 1, i - (2*n) - 2, i - (3*n) - 3]  
blockIndexes n i Grider.Left = [i, i - 1, i - 2, i - 3]  
blockIndexes n i Grider.DownLeft = [i, i + n - 1, i + (2*n) - 2, i + (3*n) - 3]  
blockIndexes n i Grider.Down = [i, i+n, i+ (2*n), i + (3*n)]  
blockIndexes n i Grider.DownRight = [i, i + n + 1, i + (2*n) + 2, i + (3*n) + 3]  
blockIndexes n i Grider.Right = [i, i + 1, i + 2, i + 3]  
blockIndexes n i Grider.UpRight = [i, i - n + 1, i - (2*n) + 2, i - (3*n) + 3]  

safeGet :: Int -> IndexedGrid -> Int
safeGet i grid = case M.lookup i grid of
    Nothing -> 1
    Just x -> x

generateBlock :: Size -> IndexedGrid -> Int -> Direction -> Block
generateBlock size grid index direction = map (\i -> safeGet i grid) indexes
    where indexes = blockIndexes size index direction

generateBlocks :: Size -> IndexedGrid -> Blocks
generateBlocks n grid = M.foldlWithKey (\acc key val -> 
    let newblocks = map (\direction -> generateBlock n grid key direction) directions in
        concat [acc, newblocks]) [] grid 
    where directions = [Grider.Up , Grider.UpLeft , Grider.Left , Grider.DownLeft , Grider.Down , Grider.UpRight , Grider.Right , Grider.DownRight]

getBlocks :: Grid -> Size -> IO Blocks 
getBlocks grid size = return $ generateBlocks size (mapGrid grid)
