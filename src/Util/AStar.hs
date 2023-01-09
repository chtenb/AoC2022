module Util.AStar (astar) where

{- ORMOLU_DISABLE -}
-- import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Debug.Trace (trace, traceShow)
import Data.PSQueue (PSQ, Binding((:->)))
import Data.PSQueue as PSQ
import Data.List as List
import Data.Maybe (isNothing)
{- ORMOLU_ENABLE -}

{-

This module contains an implementation of an A* search algorithm variant.
It finds the node t with the lowest associated cost that satifies the termination condition.
There must be no loops in the graph which have a net negative cost.
The given a cost function g which assigns costs to nodes and a heuristic h which estimates (g(t) - g(x)),
it must be the case that their sum f is admissable, meaning that
  f(x) = g(x) + h(x) <= g(t) for all nodes x and all terminal nodes t reachable from x
  f(t) = g(t) for all terminal nodes t
This equivalent to saying that h is optimistic about its estimation:
  h(x) <= g(t) - g(x)
  h(t) = 0

Moreover, f is called consistent if
  f(x) <= f(y) for all nodes x and for any successor y of x
It follows by definition that f is automatically consistent if g and h are also consistent.

To find the node with the highest score instead, we must negate all cost quantities in the algorithm.
Otherwise a max heap should be used as queue, instead of a min heap, and the inequalities above then become
  f(x) = g(x) + h(x) >= g(t) for all nodes x and all terminal nodes t reachable from x
  f(t) = g(t)
  f(x) >= f(y) for all nodes x and for any successor y of x

-}

type Path n = [n]

type Heuristic n c = n -> c

type Successor n c = (n, c) -> [(n, c)]

type Terminator n = n -> Bool

type CostMap n c = HashMap n c

-- Visited, but not expanded
type Front n c = PSQ n c

astar :: (Ord n, Hashable n, Ord c, Monoid c) => Successor n c -> Heuristic n c -> Terminator n -> n -> (Maybe n, CostMap n c)
astar successor heuristic terminator start = astarRec successor heuristic terminator (HashMap.singleton start mempty, PSQ.singleton start mempty)

astarRec :: (Ord n, Hashable n, Ord c, Monoid c) => Successor n c -> Heuristic n c -> Terminator n -> (CostMap n c, Front n c) -> (Maybe n, CostMap n c)
astarRec successor heuristic terminator state@(costMap, front) =
  case PSQ.minView front of
    Nothing -> (Nothing, costMap)
    Just (parent :-> _, remainingFront) ->
      if terminator parent
        then (Just parent, costMap)
        else astarRec successor heuristic terminator newState
      where
        parentCost = costMap HashMap.! parent
        newState = List.foldl' (processChild heuristic) (costMap, remainingFront) (successor (parent, parentCost))

processChild :: (Ord n, Hashable n, Ord c, Monoid c) => Heuristic n c -> (CostMap n c, Front n c) -> (n, c) -> (CostMap n c, Front n c)
processChild heuristic state@(costMap, front) (child, childCost) =
  let childTerminalCostEstimate = childCost <> heuristic child
      updatedState =
        ( HashMap.insert child childCost costMap,
          PSQ.insert child childTerminalCostEstimate front
        )
   in case HashMap.lookup child costMap of
        Nothing -> updatedState
        Just currentCost -> if childCost < currentCost then updatedState else state
