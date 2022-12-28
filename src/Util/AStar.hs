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

type Heuristic n = n -> Double

type Successor n = n -> [(n, Double)]

type Visited n = Map n Double

-- Visited, but not expanded
type Front n = PSQ n Double

astar :: (Ord n) => Successor n -> Heuristic n -> n -> n -> Visited n
astar successor heuristic start = astarRec successor heuristic (Map.singleton start 0, PSQ.singleton start 0)

astarRec :: (Ord n) => Successor n -> Heuristic n -> (Visited n, Front n) -> n -> Visited n
astarRec successor heuristic state@(visited, front) end =
  let newState@(newVisited, newFront) =
        case PSQ.minView front of
          Nothing -> error "front was empty"
          Just (parent :-> _, remainingFront) ->
            let parentCost = visited ! parent
             in List.foldl' (processChild heuristic end parentCost) (visited, remainingFront) (successor parent)
   in if PSQ.null newFront then newVisited else astarRec successor heuristic newState end

processChild :: (Ord n) => Heuristic n -> n -> Double -> (Visited n, Front n) -> (n, Double) -> (Visited n, Front n)
processChild heuristic end parentCost state@(visited, front) (child, stepCost) =
  let childCost = parentCost + stepCost
      childTotalCostEstimate = childCost + heuristic child
      updatedState =
        ( Map.insertWith min child childCost visited,
          PSQ.insertWith min child childTotalCostEstimate front
        )
   in case Map.lookup child visited of
        Nothing -> updatedState
        Just currentCost -> if childCost < currentCost then updatedState else state
