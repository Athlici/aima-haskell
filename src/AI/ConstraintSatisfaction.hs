{-# LANGUAGE MultiParamTypeClasses #-}

module AI.ConstraintSatisfaction where

import Control.Monad.State
import Data.Map (Map, (!))

import qualified Data.List as L
import qualified Data.Map as M

import AI.Util.Queue
import AI.Util.Util

-- |Data type used for domains.
type Domain a b = Map a [b]

-- |Data type used for neighbor lists.
type Neighbour a = Map a [a]

-- |Data type used for assignments.
type Assignment var val = Map var val

-- |This class describes finite-domain Constraint Satisfaction Problems.
--  A CSP is specified by the following three inputs:
--      vars        A list of variables; each is atomic (eg Int or String)
--      domains     A map of (var, val) entries
--      neighbours  A map of (var, [var]) that for each variable lists the
--                  other variables that participate in the constraints.
--      constraints A function @f A a B b@ that returns @True@ if neighbours
--                  A, B satisfy the constraint when @A == a@ and @B == b@.
--  In the textbook and in most mathematical definitions, the constraints are
--  specified as explicit pairs of allowable values, but the formulation here
--  is easier to express and more compact in most cases. (For example, the
--  n-Queens problem can be represented in O(n) space using this notation,
--  instead of O(n^4) for the explicit representation.) In terms of describing
--  the CSP as a problem, that's all there is.
--
--  However, the class also supports data structures and methods that help you
--  solve CSPs by calling a search function on the CSP. Methods and slots are
--  as follows, where the argument @a@ represents an assignment, which is a
--  map of (var, val) entries.
class Ord var => CSP c var val where

    -- | A list of variables.
    vars :: c var val -> [var]

    -- | A mapping of variables to possible values.
    domains :: c var val -> Domain var val

    -- | A mapping of variables to a list of the other variables that
    --   participate in its constraints.
    neighbours :: c var val -> Neighbour var

    -- | A function @f A a B b@ that returns True if neighbours A, B satisfy
    --   the constraint when they have values A == a and B == b.
    constraints :: c var val -> var -> val -> var -> val -> Bool

    -- | Return the number of conflicts that var == val has with other
    --   variables currently assigned.
    nConflicts :: c var val -> var -> val -> Assignment var val -> Int
    nConflicts csp var val assignment = countIf conflict assignedVals
        where
            assignedVals   = M.toList assignment
            conflict (x,y) = not (constraints csp var val x y)

    -- | Do forward checking (current domain reduction) for this assignment.
    forwardCheck :: c var val -> var -> val -> Map var val -> ()
    forwardCheck = undefined

    -- Return a list of (action, state) pairs.
    -- succ :: c var val -> 

    -- | The goal is to assign all vars with all constraints satisfied.
    goalTest :: c var val -> Assignment var val -> Bool
    goalTest csp assignment =
        M.size assignment == length (vars csp) && all noConflicts (vars csp)
        where
            noConflicts v = 
                nConflicts csp v (assignment ! v) assignment == 0


-- |Add (var, val) to a map of current assignments, discarding the old
--  value if any.
assign :: Ord var => var -> val -> Assignment var val -> Assignment var val
assign = M.insert

-- |Remove (var, val) from assignments, i.e. backtrack. Do not call this
--  if you are assigning var to a new value - just call 'assign' for that.
unassign :: Ord var => var -> Assignment var val -> Assignment var val
unassign = M.delete

--------------------------------------
-- Constraint Propagation with AC-3 --
--------------------------------------

-- |The arc-consistency algorithm AC-3 to reduce the domains of a constraint
--  satisfaction problem until they are arc-consistent. A @Bool@ flag is also
--  returned, with the value @False@ if an inconsistency is found and @True@ 
--  otherwise.
ac3 :: CSP c var val => c var val -> Maybe (Domain var val)
ac3 csp = go (domains csp) initial
    where
        -- |Initial queue of variable pairs to be tested.
        initial = [ (x, y) | x <- vars csp, y <- neighbours csp ! x ]

        -- |The main recursive function, which keeps track of the current
        --  works queue and the restricted domains.
        go dom queue
            | empty queue = Just dom
            | not revised = go dom rest 
            | otherwise   = if null (dom' ! x)
                                then Nothing
                                else go dom' queue'
            where
                ((x,y), rest)   = pop queue
                (revised, dom') = removeInconsistentValues dom x y
                queue'          = extend newElts rest
                newElts = map (\z -> (z,x)) (L.delete y (neighbours csp ! x))

        -- |Returns a new domain for x, together with a Bool flag indicating
        --  whether the domain has been revised or not.
        removeInconsistentValues dom x y = if length new < length old
            then (True,  M.insert x new dom)
            else (False, dom)

            where
                old = dom ! x
                new = filter fun old
                fun xv = any (\yv -> constraints csp x xv y yv) (dom ! y)

---------------------
-- Search for CSPs --
---------------------

backtrackingSearch :: CSP c var val => c var val -> Maybe (Assignment var val)
backtrackingSearch csp = recursiveBacktracking csp M.empty

recursiveBacktracking :: CSP c var val =>
                         c var val
                      -> Assignment var val
                      -> Maybe (Assignment var val)
recursiveBacktracking csp assignment = if M.size assignment == length (vars csp)
    then Just assignment
    else fun vals
    where
        var  = selectUnassignedVariable csp assignment
        vals = orderDomainValues csp var assignment
        
        fun []     = Nothing
        fun (v:vs) = if nConflicts csp var v assignment == 0
            then do
                dom <- inference csp var v
                recursiveBacktracking csp (addToAssignment dom assignment)
            else do
                dom <- inference csp var v
                recursiveBacktracking csp (unassign var $ delFromAssignment dom assignment)

        selectUnassignedVariable = undefined
        orderDomainValues = undefined
        inference = undefined
        addToAssignment = undefined
        delFromAssignment = undefined

-----------------
-- Example CSP --
-----------------

data ExampleCSP a b = ExampleCSP

instance CSP ExampleCSP Char Int where
    vars _ = "XY"
    domains _ = M.fromList [ ('X', [1,2]), ('Y', [1]) ]
    neighbours _ = M.fromList [ ('X',"Y"), ('Y',"X") ]
    constraints _ x xv y yv = xv /= yv