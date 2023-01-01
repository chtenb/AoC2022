module Util.AStar where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import Data.Map.Strict as Map
import qualified Data.Map.Strict as Map
import Debug.Trace (trace, traceShow)
import Data.PSQueue (PSQ, Binding((:->)))
import Data.PSQueue as PSQ
import Data.List as List
import Data.Maybe (isNothing)
{- ORMOLU_ENABLE -}

{-
This module contains an implementation of the A* search algorithm
-}

type Path n = [n]

-- Lower bound for the remaining cost to arrive at the destination from the given node
type Heuristic n = n -> Double

type Successor n = n -> [(n, Double)]

type Terminator n = n -> Bool

type Visited n = Map n Double

-- Visited, but not expanded
type Front n = PSQ n Double

astar :: (Ord n) => Successor n -> Heuristic n -> Terminator n -> n -> (Maybe n, Visited n)
astar successor heuristic terminator start = astarRec successor heuristic terminator (Map.singleton start 0, PSQ.singleton start 0)

astarRec :: (Ord n) => Successor n -> Heuristic n -> Terminator n -> (Visited n, Front n) -> (Maybe n, Visited n)
astarRec successor heuristic terminator state@(visited, front) =
  case PSQ.minView front of
    Nothing -> error "front was empty"
    Just (parent :-> _, remainingFront) ->
      if
          | terminator parent -> (Just parent, visited)
          | PSQ.null newFront -> (Nothing, newVisited)
          | otherwise -> astarRec successor heuristic terminator newState
      where
        parentCost = visited ! parent
        newState@(newVisited, newFront) = List.foldl' (processChild heuristic parentCost) (visited, remainingFront) (successor parent)

processChild :: (Ord n) => Heuristic n -> Double -> (Visited n, Front n) -> (n, Double) -> (Visited n, Front n)
processChild heuristic parentCost state@(visited, front) (child, stepCost) =
  let childCost = parentCost + stepCost
      childTotalCostEstimate = childCost + heuristic child
      updatedState =
        ( Map.insertWith min child childCost visited,
          PSQ.insertWith min child childTotalCostEstimate front
        )
   in case Map.lookup child visited of
        Nothing -> updatedState
        Just currentCost -> if childCost < currentCost then updatedState else state
