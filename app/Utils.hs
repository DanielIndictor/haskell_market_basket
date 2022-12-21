module Utils where

import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.List (sort)

sort2dList :: Ord a => [[a]] -> [[a]]
sort2dList = sort . map sort

-- A bit obvious, but it parallelizes, so choose a good N!
-- Example:
-- > > data Node = D [Node] | S Int
-- > > nAryFold 2 D (S 0) (map S [1..4])
-- > D [D [D [S 1,S 2],D [S 3,S 4]],S 0]
nAryFold :: (NFData a) => Int -> ([a] -> a) -> [a] -> a
nAryFold _ f  [] = f []
nAryFold _ f [x] = f [x]
nAryFold n f  xs = nAryFold n f foldedChunks
  where foldedChunks = rdeepseq `parMap` f $ chunksOf n xs


-- > > growingChunks 5 [1..30]
-- > [[1],[2],[3,4],[5,6,7],[8,9,10,11,12],[13,14,15,16,17],[18,19,20,21,22],[23,24,25,26,27],[28,29,30]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (g1, gs) = splitAt n xs in g1 : chunksOf n gs


-- > > headAndTails [1,2,3,4,5]
-- > [(1,[2,3,4,5]),(2,[3,4,5]),(3,[4,5]),(4,[5]),(5,[])]
headAndTails :: [a] -> [(a, [a])]
headAndTails [] = []
headAndTails (x : xs) = (x, xs) : headAndTails xs

--
-- THE FUNCTIONS BELOW THIS LINE ARE LEFT FOR POSTERITY AND AREN'T USED
--

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just x) = Right x
maybeToEither left Nothing = Left left

-- Nothing ever changes
fib :: Integral a => [a]
fib = nextFib 1 1 where nextFib a b = a : nextFib b (a + b)

-- > > growingChunks 5 [1..30]
-- > [[1],[2],[3,4],[5,6,7],[8,9,10,11,12],[13,14,15,16,17],[18,19,20,21,22],[23,24,25,26,27],[28,29,30]]
growingChunks:: Int -> [a] -> [[a]]
growingChunks maxChunkSize = chunker splitters
  where
    splitters = splitAt <$> takeWhile (<maxChunkSize) fib
    chunker _ [] = []
    chunker [] as = chunksOf maxChunkSize as
    chunker (splitter: remSplitters) as = let (chunk, remAs) = splitter as 
                                          in chunk : chunker remSplitters remAs 

