module Main where

import Utils -- Factored out general(ish) functions for safekeeping.

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Text.Read as Read
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Tree as Tree
import Control.Parallel.Strategies
import Control.Monad.Loops (whileM)
import Data.Foldable (foldl')
import Data.List (sortOn)

--
-- Core algorithm logic below.
--

type TIDMap = IntMap.IntMap IntSet.IntSet
type ITree = Tree.Tree Int

-- Terminology below comes from max-miner paper, except for "getOrders", 
-- which stands for "the set of transactions containing the itemset represented by this node."
data ISeed = ISeed { getLastItemInHead :: !Int, getTail :: ![Int], getOrders :: !IntSet.IntSet }
  deriving (Show, Eq)


genPruneCands :: Int -> TIDMap -> ISeed -> [(Int, IntSet.IntSet)]
genPruneCands minSup tidMap (ISeed _ unChildren orders) = 
  [ (candidate, candidateOrders) 
  | candidate <- unChildren -- Generate candidates
  , let candidateOrders = IntSet.intersection orders (tidMap IntMap.! candidate)
  , minSup <= IntSet.size candidateOrders  -- Pruning step
  ]

bootstrapGenPruneCands :: Int -> TIDMap -> [(Int, IntSet.IntSet)]
bootstrapGenPruneCands minSup = filter ((>=minSup) . IntSet.size . snd) . IntMap.toAscList 

maxMinerCandReorder :: [(Int, IntSet.IntSet)] -> [(Int, IntSet.IntSet)]
maxMinerCandReorder =  sortOn (IntSet.size . snd)

packCands :: [(Int, IntSet.IntSet)] -> [ISeed]
packCands candidates = let (items, orders) = unzip candidates
  in zipWith packSeed (headAndTails items) orders
  where packSeed (item, uncheckedItems) order = ISeed item uncheckedItems order

-- This function gives an expression (search tree) for frequent itemsets.
getFreqForest :: Int -> TIDMap -> [ITree]
getFreqForest minSup tidMap = Tree.unfoldForest blowup oneSeeds
  where 
    candGenPruner = genPruneCands minSup tidMap
    oneSeeds :: [ISeed]  -- One-itemsets are the base-case for search.
    oneSeeds = packCands $ maxMinerCandReorder $ bootstrapGenPruneCands minSup tidMap 

    blowup :: ISeed -> (Int, [ISeed])
    blowup seed = (getLastItemInHead seed, (packCands . maxMinerCandReorder . candGenPruner) seed)

-- Decouple parallelization strategy from evaluation, as advised in class.
aprioriStrategy :: Strategy [ITree]
aprioriStrategy = parList stratITree
  where 
    stratITree :: Strategy ITree
    stratITree (Tree.Node rLabel children) = do 
      children' <- parListChunk 10 rdeepseq children
      return $ Tree.Node rLabel children'

-- This is a debug function that, inefficiently,
-- takes all the paths from root to leaf in the search tree
-- and filters out non-maximal itemsets. For example,
-- the itemset pair [1,2,3] and [1,3] are not maximal.
-- A maximal set of itemsets has the property that no itemset
-- is a subset of the others.
toMaximalItemsets :: [[Int]] -> [IntSet.IntSet]
toMaximalItemsets itemsets = foldl' prependIfNew [] itemsets'
  where 
    itemsets' = sortOn (\set -> ((-1)* IntSet.size set, set)) $ fmap IntSet.fromList itemsets
    prependIfNew maximals unseenISet | any (IntSet.isSubsetOf unseenISet) maximals = maximals
    prependIfNew maximals unseenISet  = unseenISet : maximals

--
-- The code below provides functions for reading in a list of transactions
-- from some file and converting them into a map from items 
-- to the transactions containing that item.
--

-- Transactions read in in chunks and converted into mini-TIDMaps, which are then finally combined with mergeTIDMaps
mergeTIDMaps :: [TIDMap] -> TIDMap
mergeTIDMaps = nAryFold n (IntMap.unionsWith IntSet.union)
  where n=50

-- This takes a list of (transaction ID, transaction) pairs and makes it into a map from transaction ID's to orders.
-- Each transaction must have distinct items in ascending order.
transposeOrders :: [(Int, [Int])] -> TIDMap
transposeOrders =  IntMap.unionsWith IntSet.union . map transposeRow
  where
    transposeRow :: (Int, [Int]) -> TIDMap
    transposeRow (tid, order) = IntMap.fromDistinctAscList [(itm, IntSet.singleton tid) | itm <- order]

-- Extracts integers from a lines of a transaction file.
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
readTIDMapFromFile :: String -> IO (Either String (TIDMap, Int))
readTIDMapFromFile filename = IO.withFile filename IO.ReadMode (\handle -> do 
  inputLines <- whileM (not <$> IO.hIsEOF handle) (B.hGetLine handle) :: IO [B.ByteString]
  let mOrders = map mkOrder inputLines :: [Maybe [Int]]
  let mTIDOrderPairs = zipWith (fmap . (,)) [1..] mOrders :: [Maybe (Int, [Int])]
  -- These are the mini-TIDMaps
  let tidmaps = withParStrat $ map (fmap transposeOrders . sequence) $ chunksOf n mTIDOrderPairs :: [Maybe TIDMap]
  let mTIDMap = mergeTIDMaps <$> sequence tidmaps :: Maybe TIDMap
  case mTIDMap of
    Just tidmap -> return $ Right (tidmap, length inputLines)
    Nothing -> return (Left err)
  ) 
  where 
    n = 1000
    withParStrat = withStrategy $ parList rdeepseq
    err = "Error: '" ++ filename ++ "' incorrectly formatted.\
          \    It should contain newline-separated transactions,\
          \    Items in the transaction must be in ascending order."


--
-- Code below is the main function and supporting characters.
--

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
     eitherErrorOrders <- readTIDMapFromFile filename
     let eitherErrorMinSup = readMinSup minSupArg

     case (,) <$> eitherErrorMinSup <*> eitherErrorOrders of
       Left err -> do 
         putStrLn err
         Exit.exitWith (Exit.ExitFailure 1)

       Right (sup, (tidmap, nOrders)) -> do
         putStrLn "got tidmap!"
         let minSupCount = minSupCountFromArg sup nOrders
         putStr "Minimum support: "
         print minSupCount
         putStrLn "The paths to leaves are:"
         let fForest = withStrategy aprioriStrategy $ getFreqForest minSupCount tidmap
         mapM_ print $ getPathsToLeaves fForest
         -- mapM_ print $ fmap IntSet.toList $ reverse $ toMaximalItemsets $ getPathsToLeaves fForest

   _usage -> do
     progName <- Environment.getProgName
     putStrLn $ "Usage: " ++ progName ++ " <filename> <minimum support [0,1]>"
     putStrLn   "    Note that the input file format is not checked."
     putStrLn   "    It should contain newline-separated transactions,"
     putStrLn   "    Items in the transaction must be in ascending order."
     Exit.exitWith (Exit.ExitFailure 1)

