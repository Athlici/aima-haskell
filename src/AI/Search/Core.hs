{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}

module AI.Search.Core (
    -- * Core classes and data structures
      Problem (..)
    , Node (..)
    , Cost
    , root
    , path
    , expand
    -- * Search algorithms
    , treeSearch
    , graphSearch
    -- * Algorithm comparison
    , compareSearchers
    , detailedCompareSearchers
    -- * Algorithm tracking
    , ProblemIO
    , mkProblemIO) where

import           Control.DeepSeq
import           Control.Monad
import           Data.IORef
import           Data.Maybe       (fromJust, listToMaybe)
import           System.IO.Unsafe

import qualified Data.Set         as S

import           AI.Util.Queue
import           AI.Util.Table
import           AI.Util.Util

--import qualified AI.Util.Graph as G

-- |The type used to represent the cost associated with a particular path.
type Cost = Double

-- |Class for an abstract problem with state type s and action type a. A
--  minimal implementation consists of 'initial' and 'successor', and one
--  of 'goal' or 'goalTest'.
class Eq s => Problem p s a where
    -- | The initial state of the problem.
    initial :: p s a -> s

    -- | Given a state, return a sequence of (action, state) pairs reachable
    --   from this state. Because of lazy evaluation we only ever compute as
    --   many elements of the list as the program needs.
    successor :: p s a -> s -> [(a, s)]

    -- | If the problem has a unique goal state, this method should return it.
    --   The default implementation of 'goalTest' compares for equality with
    --   this state.
    goal :: p s a -> s
    goal = undefined

    -- | Return true if the state is a goal. The default method compares the
    --   state to the state specified in the implementation of 'goal'. You can
    --   override this method if checking against a single goal is not enough.
    goalTest :: p s a -> s -> Bool
    goalTest p s = s == goal p

    -- | Return the cost of a solution path that arrives at the second state
    --   from the first state, via the specified action. If the problem is such
    --   that the path doesn't matter, the function will only look at the second
    --   state. The default implementation costs 1 for every step in the path.
    costP :: p s a -> Cost -> s -> a -> s -> Cost
    costP _ c _ _ _ = c + 1

    -- | You may want to specify a heuristic function for the problem. The
    --   default implementation always returns zero.
    heuristic :: p s a -> Node s a -> Cost
    heuristic _ = const 0

    -- | For optimization problems, each state has a value. Hill-climbing and
    --   related algorithms try to maximise this value. The default
    --  implementation always returns zero.
    valueP :: p s a -> s -> Double
    valueP _ = const 0

---- |Extending the Problem Class for bidirectional Problems
--class Problem p s a => BidirProblem p s a where
--    -- | Inverse function for successor, returns the generating nodes.
--    precessor :: p s a -> s -> [(a,s)]

    -- goalTest?

-- |A node in a search tree. It contains a reference to its parent (the node
--  that this is a successor of) and to the state for this node. Note that if
--  a state can be arrived at by two paths, there will be two nodes with the
--  same state. It may also include the action that got us to this state, and
--  the total path cost.
data Node s a = Node { state  :: s
                     , parent :: Maybe (Node s a)
                     , action :: Maybe a
                     , cost   :: Cost
                     , depth  :: Int
                     , value  :: Double }

instance (Show s, Show a) => Show (Node s a) where
    show (Node state _ action cost depth _) =
        "Node(state=" ++ show state ++ ",action=" ++ show action ++
            ",cost=" ++ show cost ++ ",depth=" ++ show depth ++ ")"

-- Ignores actions; primarily because it's messy and also because two actions which
-- lead from the same parent to the same node are redundant anyway
instance Eq s => Eq (Node s a) where
--    (Node as ap _ ac ad av) == (Node bs bp _ bc bd bv) = (as,ap,ac,ad,av) == (bs,bp,bc,bd,bv)
    (Node a _ _ _ _ _) == (Node b _ _ _ _ _) = a == b

instance Ord s => Ord (Node s a) where
--    (Node as ap _ ac ad av) `compare` (Node bs bp _ bc bd bv) = (as,ap,ac,ad,av) `compare` (bs,bp,bc,bd,bv)
    (Node a _ _ _ _ _) `compare` (Node b _ _ _ _ _) = a `compare` b

-- |A convenience constructor for root nodes (a node with no parent, no action
--  that leads to it, and zero cost.)
root :: (Problem p s a) => p s a -> Node s a
root p = Node s Nothing Nothing 0 0 (valueP p s) where s = initial p

-- |Create a list of paths from the root node to the node specified.
path :: Node s a -> [Node s a]
path n = case parent n of
    Nothing -> [n]
    Just n' -> n : path n'

-- |Return a list of nodes reachable from this node in the context of the
--  specified problem.
expand :: (Problem p s a) => p s a -> Node s a -> [Node s a]
expand p node = [ mkNode a s | (a,s) <- successor p (state node) ]
    where
        mkNode a s = Node s (Just node) (Just a) (c a s) (1 + depth node) v
        c          = costP p (cost node) (state node)
        v          = valueP p (state node)

----------------------------
-- Core Search Algorithms --
----------------------------

-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. We don't worry about repeated paths
--  to a state.
treeSearch :: (Problem p s a, Queue q) =>
              q (Node s a)      -- ^ Empty queue
           -> p s a             -- ^ Problem
           -> [Node s a]
treeSearch q prob  = genericSearch f q prob
    where
        f node closed = (expand prob node,closed)

-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. If two paths reach the same state, use
--  only the best one.
graphSearch :: (Problem p s a, Queue q, Ord s) =>
               q (Node s a)     -- ^ Empty queue
            -> p s a            -- ^ Problem
            -> [Node s a]
graphSearch q prob = genericSearch f q prob
    where
        f node closed
            | state node `S.member` closed  = (newQueue,closed)
            | otherwise = (expand prob node, closed')
                where
                    closed' = state node `S.insert` closed


genericSearch :: (Queue q, Problem p s a) =>
                       (Node s a -> S.Set a1 -> ([Node s a], S.Set a1))
                       -> q (Node s a) -> p s a -> [Node s a]
genericSearch f q prob = findFinalState (genericSearchPath f (root prob `push` q))
    where
        findFinalState = filter (goalTest prob.state)

-- Return a (potentially infinite) list of nodes to search.
-- Since the result is lazy, you can break out early if you find a result.
genericSearchPath :: Queue q => (a -> S.Set a1 -> ([a], S.Set a1)) -> q a -> [a]
genericSearchPath f q  = go (q,S.empty)
    where
    go (fringe,closed)
            | empty fringe = []
            | otherwise = go'  (pop fringe) closed
    go' (node, rest) closed
            | (new,closed') <- f node closed = node : go (new `extend` rest, closed')  --What is happening here?


-----------------------
-- Compare Searchers --
-----------------------

-- |Wrapper for a problem that keeps statistics on how many times nodes were
--  expanded in the course of a search. We track the number of times 'goalCheck'
--  was called, the number of times 'successor' was called, and the total number
--  of states expanded.
data ProblemIO p s a = PIO
    { problemIO     :: p s a
    , numGoalChecks :: IORef Int
    , numSuccs      :: IORef Int
    , numStates     :: IORef Int }

-- |Construct a new ProblemIO, with all counters initialized to zero.
mkProblemIO :: p s a -> IO (ProblemIO p s a)
mkProblemIO p = do
    i <- newIORef 0
    j <- newIORef 0
    k <- newIORef 0
    return (PIO p i j k)

-- |Make ProblemIO into an instance of Problem. It uses the same implementation
--  as the problem it wraps, except that whenever 'goalTest' or 's'
instance (Problem p s a, Eq s, Show s) => Problem (ProblemIO p) s a where
    initial (PIO p _ _ _) = initial p

    goalTest (PIO p n _ _) s = unsafePerformIO $ do
        modifyIORef n (+1)
        return (goalTest p s)

    successor (PIO p _ n m) s = unsafePerformIO $ do
        let succs = successor p s
        modifyIORef n (+1)
        modifyIORef m (+length succs)
        return succs

    costP (PIO p _ _ _) = costP p

    heuristic (PIO p _ _ _) = heuristic p

-- |Given a problem and a search algorithm, run the searcher on the problem
--  and return the solution found, together with statistics about how many
--  nodes were expanded in the course of finding the solution.
testSearcher :: p s a -> (ProblemIO p s a -> [Node s a]) -> IO (Maybe (Node s a),Int,Int,Int)
testSearcher prob searcher = do
    p@(PIO _ numGoalChecks numSuccs numStates) <- mkProblemIO prob
    let result = (listToMaybe $ searcher p) in result `seq` do
        i <- readIORef numGoalChecks
        j <- readIORef numSuccs
        k <- readIORef numStates
        return (result, i, j, k)

-- |NFData instance for search nodes.
instance (NFData s, NFData a) => NFData (Node s a) where
    rnf (Node state parent action cost depth value) =
        state `seq` parent `seq` action `seq`
        cost `seq` depth `seq` value `seq`
        Node state parent action cost depth value `seq` ()

-- |Run a search algorithm over a problem, returning the time it took as well
--  as other statistics.
testSearcher' :: (NFData t) => p s a -> (ProblemIO p s a -> t) -> IO (t,Int,Int,Int,Int)
testSearcher' prob searcher = do
    p@(PIO _ numGoalChecks numSuccs numStates) <- mkProblemIO prob
    (result, t) <- timed (searcher p)
    i <- readIORef numGoalChecks
    j <- readIORef numSuccs
    k <- readIORef numStates
    return (result, t, i, j, k)

-- |Test multiple searchers on the same problem, and return a list of results
--  and statistics.
testSearchers :: [ProblemIO p s a -> [Node s a]] -> p s a -> IO [(Maybe (Node s a),Int,Int,Int)]
testSearchers searchers prob = testSearcher prob `mapM` searchers

-- |Given a list of problems and a list of searchers, run every algorithm on
--  every problem and print out a table showing the performance of each.
compareSearchers :: (Show a) =>
                    [ProblemIO p s a -> [Node s a]] -- ^ List of search algorithms
                 -> [p s a]                  -- ^ List of problems
                 -> [String]                 -- ^ Problem names
                 -> [String]                 -- ^ Search algorithm names
                 -> IO [[(Maybe (Node s a),Int,Int,Int)]]
compareSearchers searchers probs header rownames = do
    results <- testSearchers searchers `mapM` probs
    printTable 20 (map (map f) (transpose results)) header rownames
    return results
    where
        f (x,i,j,k) = SB (i,j,k)

-- |Given a problem and a list of searchers, run each search algorithm over the
--  problem until they find the first solution or exhaust the space, and print out
--  a table showing the performance of each searcher. The columns of the table indicate:
--  [Algorithm name, Depth of solution, Cost of solution, Number of goal checks,
--  Number of node expansions, Number of states expanded] .
detailedCompareSearchers ::
        [ProblemIO p s a -> [Node s a]]     -- ^ List of searchers
     -> [String]                            -- ^ Names of searchers
     -> p s a                               -- ^ Problem
     -> IO ()
detailedCompareSearchers searchers names prob = do
    result <- testSearchers searchers prob
    table  <- forM result $ \(n,numGoalChecks,numSuccs,numStates) -> do
        let d = depth $ fromJust n
        let c = round $ cost $ fromJust n
        let b = fromIntegral numStates ** (1/fromIntegral d)
        return [SB d,SB c,SB numGoalChecks,SB numSuccs,SB numStates,SB b]
    printTable 20 table header names
    where
        header = ["Searcher","Depth","Cost","Goal Checks","Successors",
                  "States","Eff Branching Factor"]
