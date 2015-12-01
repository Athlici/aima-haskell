{-# LANGUAGE MultiParamTypeClasses #-} --{-# LANGUAGE FlexibleInstances #-}

--module AI.Search.Example.NPuzzle where

--ToDo:Fix imports
import AI.Search.Core
import AI.Search.Informed
import AI.Search.Uninformed

import Data.Function (on)

import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad
import Control.Monad.ST
import Data.Permute
import Data.Permute.ST
--import Math.Algebra.Group.PermutationGroup

import System.IO

----------------------
-- N Puzzle Problem --
----------------------

-- |Data structure to define an N-Puzzle problem (the problem is defined by
--  the length of the board).
data NPuzzle s a = NP { sizeNP :: Int , initialNP :: [Int]} deriving (Show)

data NPMove = Ri | Do | Le | Up deriving (Show,Eq,Enum,Ord)

data NPState = NPS { boardNP :: Permute , movesNP :: [NPMove] } deriving (Show)

instance Eq NPState where
  (NPS a _) == (NPS b _) = a == b

instance Ord NPState where
  (NPS a _) `compare` (NPS b _) = a `compare` b

instance Ord Permute where
  compare = compare `on` elems

inbound :: Int -> Int -> NPMove -> Bool
inbound n m Ri = (m `mod` n) /= n-1
inbound n m Do = m < n*(n-1) 
inbound n m Le = (m `mod` n) /= 0
inbound n m Up = m >= n

-- |N-Puzzle is an instance of Problem. 
instance Problem NPuzzle NPState NPMove where
    initial (NP n i) = NPS (inverse $ listPermute (n*n) i) []    --fromList creates a permutation sorting i, might need the inverse

    successor (NP n _) (NPS b m) = [(x,NPS (move x) (x:m)) | x <- [Ri .. Up], valid x] where
        blnkpos = b `at` 0
        valid   = inbound n blnkpos
        move  m = runST $ do
            b' <- unsafeThaw b
            p <- newCopyPermute b'
            unsafeSwapElems p 0 (indexOf b $ newpos m)
            unsafeFreeze p where
                newpos Ri = blnkpos+1
                newpos Do = blnkpos+n
                newpos Le = blnkpos-1
                newpos Up = blnkpos-n

    goalTest (NP n _) (NPS b _) = b == permute (n*n)

    heuristic (NP n _) (Node (NPS b m) _ _ _ _ _) = 
        fromIntegral . sum $ map manhattendist [1..n*n-1] where
            manhattendist i = abs (x i - xs i) + abs (y i - ys i) where
                x i = i `mod` n
                y i = i `div` n
                xs i = pos i `mod` n
                ys i = pos i `div` n
                pos i = b `at` i

--subproblems8 :: (Problem p s a) => p s a -> [s -> s]
--subproblems8 _ = [id]

--bfpdbgen

-- The depth 26 example 8Puzzle problem from page 103
puzzle8 :: NPuzzle NPState NPMove
puzzle8 = NP 3 [7,2,4,5,0,6,8,3,1]
--puzzle8 = NP 3 [1,2,0,3,4,5,6,7,8]
--puzzle8 = NP 3 [0,4,2,1,3,5,6,7,8]

main = print . show $ iterativeDeepeningAStar puzzle8
--main = do
--    hSetBuffering stdout NoBuffering
--    print $ stateSpaceExploration (NP 4 [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] :: NPuzzle NPState NPMove)

--very ugly code to map a permutation to it's lexicographic index 
--factorials from n to 0 as a list.
--factorials :: Int -> [Integer]
--factorials n = reverse $ scanl (*) 1 [1..(fromIntegral n)]

--toIndex :: Int -> Permutation Int -> Integer
--toIndex n p = fst $ foldl f (0,S.empty) $ zip (map (.^ p) [0..n-1]) (factorials (n-1)) where
--    f (i,s) (j,k) = (\sn -> (i + k * fromIntegral (j - S.findIndex j sn), sn)) $ S.insert j s

--fromIndex :: Int -> Integer -> Permutation Int
--fromIndex n i = g $ foldl f (i,S.fromList [0..n-1],[]) (factorials (n-1)) where
--    f (j,s,l) k = (\(d,m) -> (m,fromIntegral d `S.deleteAt` s,(fromIntegral d `S.elemAt` s):l)) $ divMod j k
--    g (_,_,x)   = fromPairs $ zip [n-1,n-2..0] x

