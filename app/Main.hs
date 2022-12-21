module Main where

import Utils
--import qualified MoodyIntSet

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Text.Read as Read
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.Parallel.Strategies
import Control.Monad.Loops (whileM)
import Data.Tree

type TIDMap = IntMap.IntMap IntSet.IntSet

type ITree = Tree Int

data ISeed = ISeed { thisInt :: !Int, getUncheckedChildren :: ![Int], getOrders :: !IntSet.IntSet }
  deriving (Show, Eq)

aprioriStrategy :: Strategy [ITree]
aprioriStrategy = parList stratITree
  where 
    stratITree :: Strategy ITree
    stratITree (Node rLabel children) = do 
      children' <- parListChunk 25 rdeepseq children
      return $ Node rLabel children'

getPathsToLeaves :: [Tree a] -> [[a]]
getPathsToLeaves = concatMap (foldTree f)
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

   _usage -> do
     progName <- Environment.getProgName
     putStrLn $ "Usage: " ++ progName ++ " <filename> <minimum support [0,1]>"
     putStrLn   "    Note that the input file format is not checked."
     putStrLn   "    It should contain newline-separated transactions,"
     putStrLn   "    Items in the transaction must be in ascending order."
     Exit.exitWith (Exit.ExitFailure 1)

