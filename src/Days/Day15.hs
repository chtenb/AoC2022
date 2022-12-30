module Days.Day15 (runDay) where

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
import Util.Util
{- ORMOLU_ENABLE -}

import Control.Applicative ((<$>))
import Control.Monad.State (State, evalState, get, put)
import System.Random (Random (randomR), StdGen, mkStdGen, random)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  sensorsAndBeacons <- parseSensorAndBeacon `sepBy` "\n"
  return $ Input {beacons = snd <$> sensorsAndBeacons, sensors = sortByKey radius $ fst <$> sensorsAndBeacons}

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (a, b) (c, d) = abs (fromIntegral a - fromIntegral c) + abs (fromIntegral b - fromIntegral d)

parseSensorAndBeacon :: Parser (Sensor, Position)
parseSensorAndBeacon = do
  string "Sensor at "
  sensorPos <- parsePosition
  string ": closest beacon is at "
  beaconPos <- parsePosition
  let radius = manhattanDistance sensorPos beaconPos
  return (Sensor sensorPos radius, beaconPos)

parsePosition :: Parser Position
parsePosition = do
  string "x="
  x <- signed decimal
  string ", y="
  y <- signed decimal
  return (x, y)

------------ TYPES ------------
data Input = Input {beacons :: [Position], sensors :: [Sensor]} deriving (Show, Eq)

type OutputA = Int

type OutputB = Position

-- (x,y)
type Position = (Int, Int)

data Sensor = Sensor {pos :: Position, radius :: Int} deriving (Show, Eq)

isInRadius pos sensor = manhattanDistance pos sensor.pos <= sensor.radius

maxCoord = 4000000

------------ PART A ------------
partA :: Input -> OutputA
partA input = error ""

-- let minX = minimum $ (\sensor -> fst sensor.pos - sensor.radius) <$> input.sensors
--     maxX = maximum $ (\sensor -> fst sensor.pos + sensor.radius) <$> input.sensors
--     positionsToConsider = (,2000000) <$> [minX .. maxX]
--  in length $ filter (canNoBeaconExistAtPosition input) positionsToConsider

-- Criterions:
-- 1. There exists no known beacon at the position
-- 2. There exists a sensor that has the position within its radius
canNoBeaconExistAtPosition :: Input -> Position -> Bool
canNoBeaconExistAtPosition input pos =
  let isNotBeacon = notElem pos input.beacons
      isCoveredBySensor = any (isInRadius pos) input.sensors
   in isNotBeacon && isCoveredBySensor

------------ PART B ------------
partB :: Input -> OutputB
partB input = randomRestartHillClimbing neighborhood h
  where
    h = height input.sensors

-- Strategy: random restart hillclimbing where height is defined as distance to closest sensor
height :: [Sensor] -> Position -> Int
height [] _ = maxBound -- 2147483647 -- Infinity
height (s : ss) pos =
  let d = (manhattanDistance pos s.pos) - s.radius
   in -- if d <= s.radius then d else
      min d (height ss pos)

neighborhood :: Position -> [Position]
neighborhood (x0, y0) =
  [ (x, y)
    | x <- [x0 - 1 .. x0 + 1],
      x >= 0,
      x <= maxCoord,
      y <- [y0 - 1 .. y0 + 1],
      y >= 0,
      y <= maxCoord,
      (x, y) /= (x0, y0)
  ]

-- TODO: this is strict hillclimbing. Can we relax this without running into loops?
hillClimbStep :: (Position -> [Position]) -> (Position -> Int) -> (Position, Int) -> Maybe (Position, Int)
hillClimbStep neighborhood height (currentPos, currentHeight) =
  let neighbors = map (\pos -> (height pos, pos)) (neighborhood currentPos)
      (bestHeight, bestPos) = maximum neighbors
   in if bestHeight <= currentHeight then Nothing else Just (bestPos, bestHeight)

hillClimbTrail :: (Position -> [Position]) -> (Position -> Int) -> Position -> [(Position, Int)]
hillClimbTrail neighborhood height currentPos = case hillClimbStep neighborhood height (currentPos, height currentPos) of
  Nothing -> [(currentPos, height currentPos)]
  Just next -> (currentPos, height currentPos) : hillClimbTrail neighborhood height (fst next)

hillClimb :: (Position -> [Position]) -> (Position -> Int) -> Position -> (Position, Int)
hillClimb neighborhood height currentPos = case hillClimbStep neighborhood height (currentPos, height currentPos) of
  Nothing -> (currentPos, height currentPos)
  Just next -> hillClimb neighborhood height (fst next)

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

randomInt :: Int -> Int -> R Int
randomInt lower upper = do
  gen <- get
  let (r, gen') = randomR (lower, upper) gen
  put gen'
  return r

randomPosition :: R Position
randomPosition = do
  x <- randomInt 0 maxCoord
  y <- randomInt 0 maxCoord
  return (x, y)

randomRestartHillClimbing :: (Position -> [Position]) -> (Position -> Int) -> Position
randomRestartHillClimbing neighborhood height = runRandom loop 0
  where
    loop = do
      start <- randomPosition
      let (endPos, endHeight) = hillClimb neighborhood height start
      if endHeight == 1 then return endPos else loop
