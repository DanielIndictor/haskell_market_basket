module Main where

import qualified Text.Read as Read
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.DeepSeq (($!!))
import Data.Tree

type TIDMap = IntMap.IntMap IntSet.IntSet

data ILabel = ILabel 
  { getItemID :: !Int
  , getParent :: !(Maybe (Tree ILabel))
  , getOrders :: !IntSet.IntSet
  }
  deriving (Eq)

instance Show ILabel where
  show label = show (getItemID label, getItemID label)

type ITree = Tree ILabel

getFreqTree :: Int -> TIDMap -> [ITree]
getFreqTree minSup tidMap = rootItemsets
  where 
    transactions = IntMap.elems tidMap
    allItems = IntSet.unions transactions
    rootItemsets = unfoldForest to_1_itemset transactions
    to_1_itemset items = (ILabel {getParent=Nothing, getItemID=IntSet.size items, getOrders=items}, [])

-- This takes a list of transactions and makes it into a map from transaction ID's to orders.
-- Each transaction must have distinct items in ascending order.
transposeOrders :: [[Int]] -> TIDMap
transposeOrders _ = error "Implementation below is wrong and dumb."
-- transposeOrders = IntMap.fromDistinctAscList . zip [0..] . map IntSet.fromDistinctAscList

mkOrders :: String -> Maybe [[Int]]
mkOrders = mapM orderFromLine . lines
  where orderFromLine = mapM Read.readMaybe . words

data NumericArg = ArgPercentage !Rational | ArgRawCount !Int

-- Tries to read the minimum support as a count or as a percentage of transactions.
readMinSup :: String -> Either String NumericArg
readMinSup arg = case Read.readMaybe arg of
  (Just count) | count < 0 -> Left "Error: minSup must be nonnegative!"
  (Just count) -> Right $ ArgRawCount count
  Nothing -> case Read.readMaybe arg of
    (Just frac) | frac < 0 -> Left "Error: minSup must be nonnegative!"
    (Just frac) | frac > 0 -> Left "Error: minSup must be less than one if decimal!"
    (Just frac) -> Right $ ArgPercentage frac
    Nothing -> Left "Error: Unable to interpret minSup as integer or double."

minSupCountFromArg :: NumericArg -> Int -> Int
minSupCountFromArg (ArgRawCount count) _ = count
minSupCountFromArg (ArgPercentage frac) nOrders = floor $ frac * toRational nOrders

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just x) = Right x
maybeToEither left Nothing = Left left

-- Reads orders from the file.
readOrdersFromFile :: String -> IO (Either String [[Int]])
readOrdersFromFile filename = IO.withFile filename IO.ReadMode (\handle -> do 
  contents <- IO.hGetContents handle
  let maybeOrders = mkOrders contents
  return $!! maybeToEither err (mkOrders contents)
  ) 
  where err = "Error: '" ++ filename ++ "' incorrectly formatted.\
              \    It should contain newline-separated transactions,\
              \    Items in the transaction must be in ascending order."

main = do
 args <- Environment.getArgs
 case args of 
   [filename, minSupArg] -> do
     orders <- readOrdersFromFile filename
     let sup = readMinSup minSupArg

     case (,) <$> orders <*> sup of
       Left err -> do 
         putStrLn err
         Exit.exitWith (Exit.ExitFailure 1)

       Right (orders, sup) -> do
         let minSupCount = minSupCountFromArg sup (length orders)
         let tidmap = transposeOrders orders
         print minSupCount
         print tidmap

   _usage -> do
     progName <- Environment.getProgName
     putStrLn $ "Usage: " ++ progName ++ " <filename> <minimum support [0,1]>"
     putStrLn   "    Note that the input file format is not checked."
     putStrLn   "    It should contain newline-separated transactions,"
     putStrLn   "    Items in the transaction must be in ascending order."
     Exit.exitWith (Exit.ExitFailure 1)

