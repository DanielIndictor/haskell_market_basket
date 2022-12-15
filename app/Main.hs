module Main where

import qualified Text.Read as Read
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.DeepSeq (($!!))
import Control.Parallel.Strategies
import Data.Tree

type TIDMap = IntMap.IntMap IntSet.IntSet

type ITree = Tree Int

data ISeed = ISeed { thisInt :: !Int, getUncheckedChildren :: ![Int], getOrders :: !IntSet.IntSet }
  deriving (Show, Eq, Ord)

-- > > headAndTails [1,2,3,4,5]
-- > [(1,[2,3,4,5]),(2,[3,4,5]),(3,[4,5]),(4,[5]),(5,[])]
headAndTails :: [a] -> [(a, [a])]
headAndTails [] = []
headAndTails (x : xs) = (x, xs) : headAndTails xs

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
    blowup (ISeed this unChildren orders) = (this, [
      ISeed itm childItms childOrders
      | (itm, childItms) <- headAndTails unChildren
      , let childOrders = IntSet.intersection orders (tidMap IntMap.! itm)
      , minSup <= IntSet.size childOrders
      ])

-- Example:
-- > > data Node = D Node Node | S Int
-- > > binaryFold D [S 1, S 2, S 3, S 4]
-- > [D (D (S 1) (S 2)) (D (S 3) (S 4))]
-- > > binaryFold D [S 1, S 2, S 3, S 4, S 5]
-- > [D (D (D (S 1) (S 2)) (D (S 3) (S 4))) (S 5)]
binaryFold :: (a -> a -> a) -> [a] -> [a]
binaryFold _ [] = []
binaryFold _ [x] = [x]
binaryFold f xs = binaryFold f $ collapse xs
  where 
    collapse (x1:x2:xs') = f x1 x2 : collapse xs'
    collapse xs' = xs'

-- This takes a list of transactions and makes it into a map from transaction ID's to orders.
-- Each transaction must have distinct items in ascending order.
-- transaction ID's start at 1 to match source file line numbers.
transposeOrders :: [[Int]] -> TIDMap
transposeOrders = transposeOrders1
  where
    transposeOrders1 = IntMap.unionsWith IntSet.union . binaryFold (IntMap.unionWith IntSet.union) . zipWith transposeRow [1..]
      where
        transposeRow :: Int -> [Int] -> TIDMap
        transposeRow tid order = IntMap.fromDistinctAscList $ [(itm, IntSet.singleton tid) | itm <- order]
    transposeOrders2 = IntMap.unionsWith IntSet.union . chunkEval . zipWith transposeRow [1..]
      where
        transposeRow :: Int -> [Int] -> TIDMap
        transposeRow tid order = IntMap.fromDistinctAscList $ [(itm, IntSet.singleton tid) | itm <- order]
        chunkEval = id
         --    chunkEval = withStrategy (parListChunk transposeChunkSize rpar)
        transposeChunkSize = 100
    
    transposeOrders3 = IntMap.fromListWith IntSet.union . concat . zipWith transposeRow [1..]
      where
        transposeRow :: Int -> [Int] -> [(Int, IntSet.IntSet)]
        transposeRow tid order = [(itm, IntSet.singleton tid) | itm <- order]

mkOrders :: String -> Maybe [[Int]]
mkOrders = mapM orderFromLine . lines
  where orderFromLine = mapM Read.readMaybe . words

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

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just x) = Right x
maybeToEither left Nothing = Left left

-- Reads orders from the file.
readOrdersFromFile :: String -> IO (Either String [[Int]])
readOrdersFromFile filename = IO.withFile filename IO.ReadMode (\handle -> do 
  contents <- IO.hGetContents handle
  return $!! maybeToEither err (mkOrders contents)
  ) 
  where err = "Error: '" ++ filename ++ "' incorrectly formatted.\
              \    It should contain newline-separated transactions,\
              \    Items in the transaction must be in ascending order."

main :: IO ()
main = do
 args <- Environment.getArgs
 case args of 
   [filename, minSupArg] -> do
     eitherErrorOrders <- readOrdersFromFile filename
     let eitherErrorMinSup = readMinSup minSupArg

     case (,) <$> eitherErrorOrders <*> eitherErrorMinSup of
       Left err -> do 
         putStrLn err
         Exit.exitWith (Exit.ExitFailure 1)

       Right (orders, sup) -> do
         let minSupCount = minSupCountFromArg sup (length orders)
         let tidmap = transposeOrders orders
         print minSupCount
         print $ IntMap.size tidmap
         let fForest = getFreqForest minSupCount tidmap
         putStrLn $ drawForest $ fmap (fmap show) fForest 
         print $ getMaximalPaths fForest

   _usage -> do
     progName <- Environment.getProgName
     putStrLn $ "Usage: " ++ progName ++ " <filename> <minimum support [0,1]>"
     putStrLn   "    Note that the input file format is not checked."
     putStrLn   "    It should contain newline-separated transactions,"
     putStrLn   "    Items in the transaction must be in ascending order."
     Exit.exitWith (Exit.ExitFailure 1)

