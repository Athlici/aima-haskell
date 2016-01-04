{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}

module AI.Search.Example.NPuzzle where

--ToDo:Fix imports
import           AI.Search.Core
import           AI.Search.Informed
import           AI.Search.Uninformed

import           Control.Monad.ST     (ST, runST)
import           Data.Function        (on)
import           Data.Vector          (Vector, generate, (!))

import qualified Data.List            as L
import           Data.Permute
import           Data.Permute.ST
import qualified Data.Set             as S

--import System.IO

import           System.Random

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

manhattendist :: Int -> Vector Int
manhattendist n = generate (n*n*n*n) mhd where
      mhd i = on (+) abs (ax-bx) (ay-by) where
          ax = a `mod` n
          bx = b `mod` n
          ay = a `div` n
          by = b `div` n
          a  = i `mod` (n*n)
          b  = i `div` (n*n)

mhd3 = manhattendist 3

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
        fromIntegral . sum $ map mhd [1..n*n-1] where
            mhd x = mhd3 ! (x*n*n+(b `at` x))

--subproblems8 :: (Problem p s a) => p s a -> [s -> s]
--subproblems8 _ = [id]

--bfpdbgen

-- The depth 26 example 8Puzzle problem from page 103
puzzle8 :: NPuzzle NPState NPMove
puzzle8 = NP 3 [7,2,4,5,0,6,8,3,1]
--puzzle8 = NP 3 [1,2,0,3,4,5,6,7,8]
--puzzle8 = NP 3 [0,4,2,1,3,5,6,7,8]

--main = print . show $ head $ iterativeDeepeningAStar puzzle8
--main = print . show $ recursiveBestFirstSearch puzzle8
--main = do
--    hSetBuffering stdout NoBuffering
--    print $ stateSpaceExploration (NP 4 [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] :: NPuzzle NPState NPMove)

--very ugly code to map a permutation to it's lexicographic index
--factorials from n to 0 as a list.
factorials :: Int -> [Integer]
factorials n = reverse $ scanl (*) 1 [1..(fromIntegral n)]

toIndex :: Int -> Permute -> Integer
toIndex n p = fst $ foldl f (0,S.empty) $ zip (elems p) (factorials (n-1)) where
    f (i,s) (j,k)  = newpair $ S.insert j s where
        newpair sn = (i + k * fromIntegral (j - S.findIndex j sn),sn)

fromIndex :: Int -> Integer -> Permute
fromIndex n i = res $ foldl f (i,S.fromList [0..n-1],[]) (factorials (n-1)) where
    res (_,_,x) = listPermute n $ reverse x
    f (j,s,l) k = newtriple $ divMod j k where
        newtriple (d,m) = (m,fromIntegral d `S.deleteAt` s,(fromIntegral d `S.elemAt` s):l)

toIndex' :: Int -> Permute -> Integer
toIndex' n p = rank' (toInteger n) np (inverse np) where
    rank' :: Integer -> Permute -> Permute -> Integer
    rank' 1 _ _  = 0
    rank' n p pi = res s n (rank' (n-1) p' pi') where
        res !a !b c = toInteger a + toInteger b * c --this is the most ugly fix!
        p' = runST $ unsafeThaw p  >>= \x -> unsafeSwapElems x n1 si >> unsafeFreeze x
        pi'= runST $ unsafeThaw pi >>= \x -> unsafeSwapElems x n1 s  >> unsafeFreeze x
        s  = p  `at` n1
        si = pi `at` n1
        n1 = fromInteger n-1
    np = runST $ unsafeFreeze =<< newCopyPermute =<< unsafeThaw p
        --manip f x = runST $ unsafeThaw x >>= \y -> f y >> unsafeFreeze y

fromIndex' :: Int -> Integer -> Permute
fromIndex' n i = runST $ do
    p <- newPermute n
    unrank p (toInteger n) i
    unsafeFreeze p where
        unrank :: MPermute p m => p -> Integer -> Integer -> m p
        unrank p 0 _ = return p
        unrank p n r = do
            unsafeSwapElems p (fromInteger n-1) $ fromInteger (r `mod` n)
            unrank p (n-1) (r `div` n)

--fac :: Integer -> Integer
--fac n = product [1..n]

--main = do
--    g <- newStdGen
--    l <- return $ take 10 $ randomRs (0,(fac 10000)-1) g
--    print $ l == (map ((toIndex' 10000).(fromIndex' 10000)) l)

--instance Enum Permute where
--    toEnum
--    fromEnum
--    succ
--    pred
