module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Util.AStar (astar)
import Util.Util
import Debug.Trace (trace)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  valves <- parseValve `sepBy` "\n"
  return $ Map.fromList (zip (label <$> valves) valves)

parseValve = do
  string "Valve "
  label <- parseLabel
  string " has flow rate="
  rate <- decimal
  string "; tunnel"
  option "" (string "s")
  string " lead"
  option "" (string "s")
  string " to valve"
  option "" (string "s")
  string " "
  neighbors <- parseLabel `sepBy` ", "
  return $ Valve label rate neighbors

parseLabel = do
  c1 <- satisfy $ inClass "A-Z"
  c2 <- satisfy $ inClass "A-Z"
  return [c1, c2]

------------ TYPES ------------
type Input = ValveSystem

type Label = String

data Valve = Valve {label :: Label, rate :: Int, neighbors :: [Label]} deriving (Show)

type ValveSystem = Map Label Valve

------------ PART A ------------
partA system =
  let start = spy $ initialNode system
      minutes = 26
      (end, costMap) = astar (successor system) (heuristic minutes system (maximalRate system)) (terminator minutes) start
   in (\x -> (x, costMap Map.! x)) <$> end

maximalRate system = sum $ rate <$> Map.elems system

data Node = Node {elephant :: Valve, position :: Valve, minute :: Int, openValves :: Set Label, totalRate :: Int} deriving (Show)

instance Eq Node where
  a == b = label a.position == label b.position && label a.elephant == label b.elephant && a.minute == b.minute && a.openValves == b.openValves

instance Ord Node where
  compare a b = compare (label a.position, label a.elephant, a.minute, a.openValves) (label b.position, label b.elephant, b.minute, b.openValves)

initialNode system = Node (system Map.! "AA") (system Map.! "AA") 1 Set.empty 0

data PartialSuccession = PartialSuccession {valve :: Valve, rateDelta :: Int, openedValves :: [Label]}

combinePartialSuccessions parent (succession, elephantSuccession) =
  Node
    elephantSuccession.valve
    succession.valve
    (parent.minute + 1)
    (Set.union parent.openValves $ Set.fromList $ succession.openedValves ++ elephantSuccession.openedValves)
    (parent.totalRate + succession.rateDelta + elephantSuccession.rateDelta)

-- The cost is always negative, but that is not a problem since there are no loops in the graph
successor :: ValveSystem -> Node -> [(Node, Double)]
successor system parent = (,stepCost) . combinePartialSuccessions parent <$> ([(s1, s2) | s1 <- listSuccessionsForValve parent.position, s2 <- listSuccessionsForValve parent.elephant])
  where
    -- A valve opened at a certain minute will only relieve cost in the next minute
    stepCost = -fromIntegral parent.totalRate

    listSuccessionsForValve :: Valve -> [PartialSuccession]
    listSuccessionsForValve parentValve = stayInRoom parentValve : map followTunnel parentValve.neighbors

    followTunnel neighborLabel =
      let valve = system Map.! neighborLabel
       in PartialSuccession valve 0 []

    stayInRoom valve =
      let valveIsUnopened = valve.rate > 0 && not (Set.member (label valve) parent.openValves)
          (rateDelta, openedValves) = (valve.rate, [label valve])
       in if valveIsUnopened
            then PartialSuccession valve rateDelta openedValves
            else PartialSuccession valve 0 []

terminator :: Int -> Node -> Bool
terminator totalMinutes node = node.minute == totalMinutes + 1

-- The best you can do is open all remaining valves in order of decreasing flow rate in the minutes left
heuristic :: Int -> ValveSystem -> Int -> Node -> Double
heuristic totalMinutes system maximalRate node =
  -- TODO actually implement the above
  let remainingMinutes = totalMinutes - node.minute
   in -fromIntegral (maximalRate * remainingMinutes)

------------ PART B ------------
partB :: Input -> Void
partB = error "Not implemented yet!"
