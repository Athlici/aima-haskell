{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AI.Search.Informed where

import           AI.Search.Core
import           AI.Util.Queue

import           Data.List      as L

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

--something in this is preventing depthwise lazyness
iterativeDeepeningAStar :: Problem p s a => p s a -> [Node s a]
iterativeDeepeningAStar prob = go ([],heuristic prob (root prob)) where
    go (res,lim)
      | lim == inf = res
      | otherwise  = res ++ go (ida (root prob))
        where
            ida node
              | f node > lim = ([],f node)
              | otherwise = comb $ ((s node,inf):) $ map ida $ expand prob node
            comb !x = (concatMap fst x,minimum $ map snd x)
            f node  = cost node + heuristic prob node
            s node  = [node | (lim == f node) && goalTest prob (state node)]
--              | otherwise = foldl' comb (s node,inf) $ expand prob node
--            comb (r,l) x = (\(!xr,!xl) -> (r++xr,min l xl)) $ ida x

--TODO: reformulate this to give an infinite list
recursiveBestFirstSearch :: (Problem p s a) => p s a -> [Node s a]
recursiveBestFirstSearch prob = either (:[]) (const []) $ rbfs (root prob) 0 inf where
    rbfs n f lim
      | f > lim = Right f
      | goalTest prob (state n) = Left n
      | L.null succ = Right inf
      | L.null (tail succ) = let n = head succ in rbfs n (max f (h n)) lim
      | otherwise = searchWhile $ pop (extend (map initvalues succ) (newPriorityQueue fst))
        where
            succ = expand prob n
            h n = heuristic prob n + cost n
--            initvalues = if h n<f then (\x -> (max f (h x),x)) else (\x -> (h x,x))
            initvalues x = if h n<f then (max f (h x),x) else (h x,x)     --try later whether this gets optimised
            searchWhile ((fn1,n1),r)
              | fn1 > lim || fn1 == inf = Right fn1
              | otherwise = either Left nextq (rbfs n1 fn1 bnd)
                where
                    nextq x = searchWhile $ pop $ push (x,n1) r
                    bnd = min lim (fst $ fst $ pop r)

recursiveBestFirstSearch' :: (Problem p s a) => p s a -> [Node s a]
recursiveBestFirstSearch' prob = fst $ rbfs (root prob) 0 10 where
--    rbfs :: (Problem p s a) => Node s a -> Cost -> Cost -> ([Node s a],Cost)
    rbfs node f lim
      | f > lim = ([],f)
      | L.null succ = s ([],inf)
      | L.null (tail succ) = let n = head succ in s $ rbfs n (max f (h n)) lim
      | otherwise = s $ searchWhile $ pop (extend (map initvalues succ) (newPriorityQueue snd))
        where
            s (x,y) = ([node | (h node>=f) && goalTest prob (state node)]++x,y)
            succ = expand prob node
            h n  = heuristic prob n + cost n
            initvalues x = if h node<f then (x,max f (h x)) else (x,h x)
--            searchWhile :: ((Node s a,Cost),Queue) -> ([Node s a],Cost)
            searchWhile ((n1,fn1),rq)
              | fn1 > lim || fn1 == inf = ([],fn1)
              | otherwise = comb (rbfs n1 fn1 bnd)
                where
                    comb (x,y) = (\(x1,y1) -> (x++x1,y1)) $ searchWhile $ pop $ push (n1,y) rq
                    bnd = min lim (snd $ fst $ pop rq)
