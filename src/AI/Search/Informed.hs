{-#LANGUAGE BangPatterns #-}

module AI.Search.Informed where

import AI.Search.Core
import AI.Util.Queue

import Data.List as L

inf = 1/0        --this is stupid but gives the biggest double

--TODO: Rewrite the benchmark code to use different problems and have the search
--functions use the problem heuristic by default

---------------------------------
-- Informed (Heuristic) Search --
---------------------------------

-- |Type synonym for heuristic functions. In principle they can take any
--  information at a search node into account, including cost already incurred
--  at this node, depth of the node, or the state reached so far.
type Heuristic s a = Node s a -> Double

-- |Best-first tree search takes a function that scores each potential successor
--  and prefers to explore nodes with the lowest score first.
bestFirstTreeSearch :: (Problem p s a) =>
                       Heuristic s a    -- ^ Function to score each node
                    -> p s a            -- ^ Problem
                    -> [Node s a]
bestFirstTreeSearch f = treeSearch (newPriorityQueue f)

-- |Best-first graph search keeps track of states that have already been visited
--  and won't visit the same state twice.
bestFirstGraphSearch :: (Problem p s a, Ord s) =>
                        Heuristic s a   -- ^ Function to score each node
                     -> p s a           -- ^ Problem
                     -> [Node s a]
bestFirstGraphSearch f = graphSearch (newPriorityQueue f)

-- |Minimum cost search preferentially explores nodes with the lowest cost
--  accrued, to guarantee that it finds the best path to the solution.
uniformCostSearch :: (Problem p s a, Ord s) => p s a -> [Node s a]
uniformCostSearch = bestFirstGraphSearch cost

-- |Greedy best-first search preferentially explores nodes with the lowest
--  cost remaining to the goal, ignoring cost already accrued.
greedyBestFirstSearch :: (Problem p s a, Ord s) => p s a -> [Node s a]
greedyBestFirstSearch prob = bestFirstGraphSearch (heuristic prob) prob

-- |A* search uses a heuristic function that estimates how close each state is
--  to the goal. It combines this with the path cost so far to get a total
--  score, and preferentially explores nodes with a lower score. It is optimal
--  whenever the heuristic function is admissible. 
aStarSearch :: (Problem p s a, Ord s) => p s a -> [Node s a]
aStarSearch prob = bestFirstGraphSearch (\n -> heuristic prob n + cost n) prob

-- |A variant on A* search that has the heuristic function as first argument
aStarSearch' :: (Problem p s a, Ord s) => Heuristic s a -> p s a -> [Node s a]
aStarSearch' h = bestFirstGraphSearch (\n -> h n + cost n)

iterativeDeepeningAStar :: Problem p s a => p s a -> [Node s a]
iterativeDeepeningAStar prob = go ([],heuristic prob (root prob)) where
    go (res, lim)
      | lim == inf = []
      | L.null res = go $ ida (root prob)
      | otherwise  = res
        where
            ida node
              | (cost node + heuristic prob node) > lim = ([],cost node + heuristic prob node)
              | goalTest prob (state node) = ([node],cost node)
              | otherwise = foldl' comb ([],inf) $ expand prob node
            comb (r,l) x = (\(!xr,!xl) -> (r++xr,min l xl)) $ ida x
--              | otherwise = comb $ map ida $ expand prob node
--            comb !x = (concatMap fst x,minimum $ (inf:) $ map snd x)


--this will require some thought and clever formulation as mutable nodes seem
--like an ugly solution
--recursiveBestFirstSearch :: (Problem p s a) => p s a -> [Node s a]
--recursiveBestFirstSearch prob = either (id) (const []) $ RBFS (root prob) inf where
--    RBFS n f
--        | goalTest prob n = Left [n]
--        | L.null succ = Right inf
--        | L.null (tail succ) = RBFS (head succ) f
--        | otherwise = 
--        where 
--            succ = expand prob n
