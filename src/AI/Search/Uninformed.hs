module AI.Search.Uninformed where

import AI.Search.Core
import AI.Util.Queue

import Data.List as L

----------------------------------
-- Uninformed Search Algorithms --
----------------------------------

import qualified Data.Set as S
import Data.Function (on)

stateSpaceExploration :: (Ord s,Problem p s a) => p s a -> [Int]
stateSpaceExploration prob = takeWhile (>0) (lengthlist S.empty $ S.singleton $ root prob) where 
    lengthlist p c = S.size c : lengthlist c (S.foldl' addnewelems S.empty c) where
        addnewelems s e = L.foldl' (flip S.insert) s $ L.filter unvisited $ expand prob e
        unvisited x = ((&&) `on` S.notMember x) p c

-- |Search the deepest nodes in the search tree first.
depthFirstTreeSearch :: (Problem p s a) => p s a -> [Node s a]
depthFirstTreeSearch = treeSearch []

-- |Search the shallowest nodes in the search tree first.
breadthFirstTreeSearch :: (Problem p s a) => p s a -> [Node s a]
breadthFirstTreeSearch = treeSearch (newQueue :: FifoQueue (Node s a))

-- |Search the deepest nodes in the graph first.
depthFirstGraphSearch :: (Problem p s a, Ord s) => p s a -> [Node s a]
depthFirstGraphSearch = graphSearch []

-- |Search the shallowest nodes in the graph first.
breadthFirstGraphSearch :: (Problem p s a, Ord s) => p s a -> [Node s a]
breadthFirstGraphSearch = graphSearch (newQueue :: FifoQueue (Node s a))

-- |Depth-first search with a depth limit. Returns the usual lazy list of sulutions
--  without any information whether the search failed or was cutoff.
depthLimitedSearch :: (Problem p s a) => Int -> p s a -> [Node s a]
depthLimitedSearch lim prob = recursiveDLS (root prob) where
    recursiveDLS node
      | goalTest prob (state node) = [node]
      | depth node == lim          = []
      | otherwise = concatMap recursiveDLS $ expand prob node

-- |Return type for depth-limited search. We need this as there are two types of
--  failure - either we establish that the problem has no solutions ('Fail') or
--  we can't find any solutions within the depth limit ('Cutoff').
data DepthLimited a = Fail | Cutoff | Ok a deriving (Show)

instance Eq (DepthLimited a) where
    (==) Fail   Fail   = True
    (==) Cutoff Cutoff = True
    (==) (Ok _) (Ok _) = True
    (==) _ _ = False

-- If the depth limit is reached we return 'Cutoff', otherwise return 'Fail' 
-- (if no solution is found) or 'Ok' (if a solution is found).
depthLimitedSearch' :: (Problem p s a) => Int -> p s a -> DepthLimited [Node s a] 
depthLimitedSearch' lim prob = if L.null res then cf else Ok res where
    (res,cf) = recursiveDLS (root prob) where
        recursiveDLS node
          | goalTest prob (state node) = ([node],Cutoff)      --Because the stepcost is 1 testing before
          | depth node == lim          = ([],Cutoff)          --checking the depth makes sense here
          | otherwise = comb $ map recursiveDLS $ expand prob node
        comb x = (\y -> (y,if L.null y && (Cutoff `L.notElem` map snd x) then Fail else Cutoff)) $ concatMap fst x

-- |Repeatedly try depth-limited search with an increasing depth limit.
iterativeDeepeningSearch :: (Problem p s a) => p s a -> [Node s a]
iterativeDeepeningSearch prob = go 0 where
    go lim = case depthLimitedSearch' lim prob of
        Cutoff -> go (lim + 1)
        Fail   -> []
        Ok n   -> n
