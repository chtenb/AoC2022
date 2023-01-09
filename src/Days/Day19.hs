module Days.Day19 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Data.Functor
import Data.Hashable (Hashable(..))
import Util.AStar (astar)
import Util.Util
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseBlueprint `sepBy` "\n"

parseBlueprint = do
  string "Blueprint " *> decimal <* ": "
  oreRobot <- string "Each ore robot costs " *> decimal <* " ore. "
  clayRobot <- string "Each clay robot costs " *> decimal <* " ore. "
  obsidianRobot <- (string "Each obsidian robot costs " *> decimal <* " ore and " >>= (\i -> decimal <&> (i,))) <* " clay. "
  geodeRobot <- (string "Each geode robot costs " *> decimal <* " ore and " >>= (\i -> decimal <&> (i,))) <* " obsidian."
  return Blueprint {oreRobot, clayRobot, obsidianRobot, geodeRobot}

------------ TYPES ------------
type Input = [Blueprint]

data Blueprint = Blueprint {oreRobot :: Int, clayRobot :: Int, obsidianRobot :: (Int, Int), geodeRobot :: (Int, Int)} deriving (Show, Eq, Ord)

data State = State
  { oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int,
    ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geode :: Int,
    minutesPast :: Int
  }
  deriving (Show)

instance Eq State where
  (State oreRobots1 clayRobots1 obsidianRobots1 geodeRobots1 ore1 clay1 obsidian1 geode1 minutesPast1) == (State oreRobots2 clayRobots2 obsidianRobots2 geodeRobots2 ore2 clay2 obsidian2 geode2 minutesPast2) =
    (oreRobots1 == oreRobots2)
      && (clayRobots1 == clayRobots2)
      && (obsidianRobots1 == obsidianRobots2)
      && (geodeRobots1 == geodeRobots2)
      && (ore1 == ore2)
      && (clay1 == clay2)
      && (obsidian1 == obsidian2)
      && (geode1 == geode2)

-- && (minutesPast1 == minutesPast2)

instance Ord State where
  compare (State oreRobots1 clayRobots1 obsidianRobots1 geodeRobots1 ore1 clay1 obsidian1 geode1 minutesPast1) (State oreRobots2 clayRobots2 obsidianRobots2 geodeRobots2 ore2 clay2 obsidian2 geode2 minutesPast2) =
    compare oreRobots1 oreRobots2
      `mappend` compare clayRobots1 clayRobots2
      `mappend` compare obsidianRobots1 obsidianRobots2
      `mappend` compare geodeRobots1 geodeRobots2
      `mappend` compare ore1 ore2
      `mappend` compare clay1 clay2
      `mappend` compare obsidian1 obsidian2
      `mappend` compare geode1 geode2

-- `mappend` compare minutesPast1 minutesPast2

instance Hashable State where
  hashWithSalt s (State oreRobots clayRobots obsidianRobots geodeRobots ore clay obsidian geode minutesPast) =
    s
      `hashWithSalt` oreRobots
      `hashWithSalt` clayRobots
      `hashWithSalt` obsidianRobots
      `hashWithSalt` geodeRobots
      `hashWithSalt` ore
      `hashWithSalt` clay
      `hashWithSalt` obsidian
      `hashWithSalt` geode

-- `hashWithSalt` minutesPast

initialState = State 1 0 0 0 0 0 0 0 0

data Cost = Cost Int Int deriving (Show, Eq)

instance Semigroup Cost where
  (Cost c r) <> (Cost c2 r2) = Cost (c + c2) (r + r2)

instance Monoid Cost where
  mempty = Cost 0 0

instance Ord Cost where
  compare (Cost c r) (Cost c2 r2) = if c == c2 then compare r r2 else compare c c2

------------ PART A ------------
-- partA input = sum $ zipWith (*) [1 ..] (map optimalNumberOfGeodes input)
partA :: p -> Void
partA input = error "disabled"

terminator :: State -> Bool
terminator s = s.minutesPast == maxMinutes

successor :: Blueprint -> (State, Cost) -> [(State, Cost)]
successor blueprint (startMinute, parentCost) =
  -- spy $
  map (\s -> (s, cost blueprint s)) $
    endMinute : builds
  where
    -- if length builds == 4
    --   then builds -- If all buildings are possible, no use not to build any
    --   else endMinute : builds

    builds =
      catMaybes
        [ buildOreRobot,
          buildClayRobot,
          buildObsidianRobot,
          buildGeodeRobot
        ]
    endMinute = simulateMinute startMinute
    buildOreRobot =
      if blueprint.oreRobot <= startMinute.ore
        then Just endMinute {ore = endMinute.ore - blueprint.oreRobot, oreRobots = endMinute.oreRobots + 1}
        else Nothing
    buildClayRobot =
      if blueprint.clayRobot <= startMinute.ore
        then Just endMinute {ore = endMinute.ore - blueprint.clayRobot, clayRobots = endMinute.clayRobots + 1}
        else Nothing
    buildObsidianRobot =
      if fst blueprint.obsidianRobot <= startMinute.ore && snd blueprint.obsidianRobot <= startMinute.clay
        then
          Just
            endMinute
              { ore = endMinute.ore - fst blueprint.obsidianRobot,
                clay = endMinute.clay - snd blueprint.obsidianRobot,
                obsidianRobots = endMinute.obsidianRobots + 1
              }
        else Nothing
    buildGeodeRobot =
      if fst blueprint.geodeRobot <= startMinute.ore && snd blueprint.geodeRobot <= startMinute.obsidian
        then
          Just
            endMinute
              { ore = endMinute.ore - fst blueprint.geodeRobot,
                obsidian = endMinute.obsidian - snd blueprint.geodeRobot,
                geodeRobots = endMinute.geodeRobots + 1
              }
        else Nothing

simulateMinute :: State -> State
simulateMinute state =
  state
    { ore = state.ore + state.oreRobots,
      clay = state.clay + state.clayRobots,
      obsidian = state.obsidian + state.obsidianRobots,
      geode = state.geode + state.geodeRobots,
      minutesPast = state.minutesPast + 1
    }

cost :: Blueprint -> State -> Cost
cost _ s = Cost (-s.geode) s.minutesPast

heuristic :: Blueprint -> State -> Cost
heuristic _ s =
  if terminator s
    then mempty
    else
      let minutesLeft = (maxMinutes - s.minutesPast)
       in Cost (-(s.geodeRobots * minutesLeft + (minutesLeft * (minutesLeft + 1) `div` 2))) minutesLeft

optimalNumberOfGeodes blueprint =
  let (result, costMap) = astar (successor blueprint) (heuristic blueprint) terminator initialState
   in case result of
        Just state -> traceShow state state.geode
        Nothing -> error "no solution"

------------ PART B ------------
partB input = product (map optimalNumberOfGeodes $ take 3 input)

maxMinutes = 32
