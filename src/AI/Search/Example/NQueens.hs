{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module AI.Search.Example.NQueens where

import qualified Data.List as L
import Data.Maybe (isJust)

import AI.Search.Core
import AI.Util.Util
import AI.Search.Uninformed

----------------------
-- N Queens Problem --
----------------------

-- |Data structure to define an N-Queens problem (the problem is defined by
--  the size of the board).
data NQueens s a = NQ { sizeNQ :: Int } deriving (Show)

-- |Update the state of the N-Queens board by playing a queen at (i,n).
updateNQ :: (Int,Int) -> [Maybe Int] -> [Maybe Int]
updateNQ (c,r) = insert c (Just r)

-- |Would putting two queens in (r1,c1) and (r2,c2) conflict?
conflict :: Int -> Int -> Int -> Int -> Bool
conflict r1 c1 r2 c2 =
    r1 == r2 || c1 == c2 || r1-c1 == r2-c2 || r1+c1 == r2+c2

-- |Would placing a queen at (row,col) conflict with anything?
conflicted :: [Maybe Int] -> Int -> Int -> Bool
conflicted state row col = any f (enumerate state)
    where
        f (_, Nothing) = False
        f (c, Just r)  = not (c == col && r == row) && conflict row col r c

-- |N-Queens is an instance of Problem. 
instance Problem NQueens [Maybe Int] (Int,Int) where
    initial (NQ n) = replicate n Nothing

    -- @L.elemIndex Nothing s@ finds the index of the first column in s
    -- that doesn't yet have a queen.
    successor (NQ n) s = case L.elemIndex Nothing s of
        Nothing -> []
        Just i  -> zip actions (map (`updateNQ` s) actions)
            where
                actions = map ((,) i) [0..n-1]

    goalTest (NQ n) s = isJust (last s) && all (not . f) (enumerate s) where
        f (c,Nothing) = False
        f (c,Just r)  = conflicted s r c

-- |An example N-Queens problem on an 8x8 grid.
nQueens :: NQueens [Maybe Int] (Int,Int)
nQueens = NQ 8

-- main = print $ depthLimitedSearch 8 nQueens
