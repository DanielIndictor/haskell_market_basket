module MoodyIntSet where

import qualified Data.Foldable as Foldable
import Control.DeepSeq (NFData, rnf)
-- import qualified Data.Vector as V

data MoodyIntSet = Sparse { size :: !Int, toList :: ![Int] }

-- | Packed !(V.Vector Int)
--

instance NFData MoodyIntSet where
  rnf (Sparse siz list) = siz `seq` rnf list


instance Show MoodyIntSet where
  show (Sparse _ s) = show s

instance Eq MoodyIntSet where
  (Sparse size1 list1) == (Sparse size2 list2) = (size1==size2) || (list1==list2)

empty :: MoodyIntSet
empty = Sparse 0 []

singleton :: Int -> MoodyIntSet
singleton x = Sparse 1 [x]

-- Precondition not checked.
fromDistinctAscList :: [Int] -> MoodyIntSet
fromDistinctAscList xs = Sparse (length xs) xs


union :: MoodyIntSet -> MoodyIntSet -> MoodyIntSet
union (Sparse _ s1) (Sparse _ s2) = let (s, u) = union' 0 s1 s2 in  Sparse s u
  where 
    union' s as@(a1:tailAs) bs@(b1:tailBs) = case a1 `compare` b1 of
      LT -> let (s', u') = union' s tailAs     bs in (s' + 1, a1:u')
      GT -> let (s', u') = union' s as     tailBs in (s' + 1, b1:u')
      EQ -> let (s', u') = union' s tailAs tailBs in (s' + 1, a1:u')
    union' s as [] = (s + length as, as)
    union' s [] bs = (s + length bs, bs)

unions :: Foldable f => f MoodyIntSet -> MoodyIntSet
unions = Foldable.foldl' union empty

intersection :: MoodyIntSet -> MoodyIntSet -> MoodyIntSet
intersection (Sparse _ s1) (Sparse _ s2) = let (s, int) = intersect' (0::Int) s1 s2 in Sparse s int
  where 
    intersect' s as@(a1:tailAs) bs@(b1:tailBs) = case a1 `compare` b1 of
      LT -> let (s', int') = intersect' s tailAs     bs in (s', int')
      GT -> let (s', int') = intersect' s as     tailBs in (s', int')
      EQ -> let (s', int') = intersect' s tailAs tailBs in (s', a1:int')
    intersect' _  _ _ = (0, [])  -- If either is empty, intersection is empty.
