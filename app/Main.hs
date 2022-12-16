module Main where

import Utils

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Text.Read as Read
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.DeepSeq (deepseq, ($!!))
import Control.Parallel.Strategies
import Control.Monad.Loops (whileM)
import Data.Tree

type TIDMap = IntMap.IntMap IntSet.IntSet

type ITree = Tree Int

data ISeed = ISeed { thisInt :: !Int, getUncheckedChildren :: ![Int], getOrders :: !IntSet.IntSet }
  deriving (Show, Eq, Ord)

aprioriStrategy :: Strategy [ITree]
aprioriStrategy = evalList rdeepseq

getMaximalPaths :: [Tree a] -> [[a]]
getMaximalPaths = concatMap (foldTree f)
  where
    f x [] = [[x]]
    f x paths = (x:) <$> concat paths

getFreqForest :: Int -> TIDMap -> [ITree]
getFreqForest minSup tidMap = rootItemsets
  where 
    rootItemsets = unfoldForest blowup oneSeeds
    items = IntMap.keys tidMap
    oneSeeds :: [ISeed]
    oneSeeds = [ISeed itm childItms childOrders 
      | (itm, childItms) <- headAndTails items
      , let childOrders =  tidMap IntMap.! itm
      , minSup <= IntSet.size childOrders
      ]
    blowup :: ISeed -> (Int, [ISeed])
    blowup (ISeed this unChildren orders) = (this, seeds)
     where 
       seeds = 
         [ ISeed itm childItms childOrders
         | (itm, childItms) <- headAndTails unChildren
         , let childOrders = IntSet.intersection orders (tidMap IntMap.! itm)
         , minSup <= IntSet.size childOrders
         ]

-- Example:
-- > > data Node = D [Node] | S Int
-- > > nAryFold 2 D (S 0) (map S [1..4])
-- > D [D [D [S 1,S 2],D [S 3,S 4]],S 0]
nAryFold :: Int -> ([a] -> a) -> a -> [a] -> a
nAryFold _ f def  [] = f [def]
nAryFold _ f def [x] = f [x, def]
nAryFold n f def  xs = nAryFold n f def foldedChunks
  where foldedChunks = rseq `parMap` f $ chunksOf n xs

-- This takes a list of transactions and makes it into a map from transaction ID's to orders.
-- Each transaction must have distinct items in ascending order.
-- transaction ID's start at 1 to match source file line numbers.
transposeOrders :: [[Int]] -> TIDMap
transposeOrders = nAryFold n (IntMap.unionsWith IntSet.union) IntMap.empty . zipWith transposeRow [1..]
  where
    n = 100
    transposeRow :: Int -> [Int] -> TIDMap
    transposeRow tid order = IntMap.fromDistinctAscList $ [(itm, IntSet.singleton tid) | itm <- order]

mkOrder :: B.ByteString -> Maybe [Int]
mkOrder = sequence . getInts
  where
    getInts :: B.ByteString -> [Maybe Int]
    getInts b | B.null b = []
    getInts b | B.head b == 32 = getInts $ B.tail b  -- ' ' == 32
    getInts b = case BC.readInt b of
      Just (i, remainder) -> Just i : getInts remainder
      Nothing -> [Nothing]

-- Reads orders from the file.
readOrdersFromFile :: String -> IO (Either String [[Int]])
readOrdersFromFile filename = IO.withFile filename IO.ReadMode (\handle -> do 
  -- IO.hGetBuffering handle >>= print
  -- IO.hSetBuffering handle $ IO.BlockBuffering (Just 123)
  -- IO.hGetBuffering handle >>= print
  inputLines <- whileM (not <$> IO.hIsEOF handle) (B.hGetLine handle) :: IO [B.ByteString]
  case sequence . withParStrat $ map mkOrder inputLines of
    Just orders -> return $!! Right orders
    Nothing -> return (Left err)
  ) 
  where 
    withParStrat = withStrategy $ parListChunk 5000 rdeepseq
    err = "Error: '" ++ filename ++ "' incorrectly formatted.\
          \    It should contain newline-separated transactions,\
          \    Items in the transaction must be in ascending order."

data NumericArg = ArgPercentage !Rational | ArgRawCount !Int

-- Tries to read the minimum support as a count or as a percentage of transactions.
readMinSup :: String -> Either String NumericArg
readMinSup arg = case Read.readMaybe arg of
  (Just count) | count < 0 -> Left "Error: minSup must be nonnegative!"
  (Just count) -> Right $ ArgRawCount count
  Nothing -> case (Read.readMaybe arg :: Maybe Double) of
    (Just frac) | frac < 0 -> Left "Error: minSup must be nonnegative!"
    (Just frac) | frac > 1 -> Left "Error: minSup must be less than one if decimal!"
    (Just frac) -> Right $ ArgPercentage $ toRational frac
    Nothing -> Left "Error: Unable to interpret minSup as integer or double."

minSupCountFromArg :: NumericArg -> Int -> Int
minSupCountFromArg (ArgRawCount count) _ = count
minSupCountFromArg (ArgPercentage frac) nOrders = floor $ frac * toRational nOrders


main :: IO ()
main = do
 args <- Environment.getArgs
 case args of 
   [filename, minSupArg] -> do
     eitherErrorOrders <- readOrdersFromFile filename
     let eitherErrorMinSup = readMinSup minSupArg

     case (,) <$> eitherErrorMinSup <*> eitherErrorOrders of
       Left err -> do 
         putStrLn err
         Exit.exitWith (Exit.ExitFailure 1)

       Right (sup, orders) -> do
         let minSupCount = minSupCountFromArg sup (length orders)
         print minSupCount
         
         print $ orders `deepseq` "evaluated orders!"
         -- let tidmap = transposeOrders orders
         -- print minSupCount
         -- print $ IntMap.size tidmap
         -- let fForest = withStrategy aprioriStrategy $ getFreqForest minSupCount tidmap
         -- mapM_ print $ getMaximalPaths fForest

   _usage -> do
     progName <- Environment.getProgName
     putStrLn $ "Usage: " ++ progName ++ " <filename> <minimum support [0,1]>"
     putStrLn   "    Note that the input file format is not checked."
     putStrLn   "    It should contain newline-separated transactions,"
     putStrLn   "    Items in the transaction must be in ascending order."
     Exit.exitWith (Exit.ExitFailure 1)

