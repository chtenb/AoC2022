{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Days.Day14 (runDay) where

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
import Data.Attoparsec.Text hiding (takeWhile)
import Data.Void
import Data.Array.ST
import Control.Monad.ST
import Data.Bifunctor (bimap)
import Control.Monad (foldM, foldM_, replicateM_)
import Util.Util
import Data.Array
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseRockPath `sepBy` "\n"
  where
    parseRockPath = parseCoord `sepBy` " -> "
    parseCoord = do
      x <- decimal
      string ","
      y <- decimal
      return $ Coord x y

------------ TYPES ------------
data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

toTuple c = (c.x, c.y)

fromTuple (x, y) = Coord x y

down c = Coord c.x (c.y + 1)

downRight c = Coord (c.x + 1) (c.y + 1)

downLeft c = Coord (c.x - 1) (c.y + 1)

instance Ord Coord where
  compare a b = compare (toTuple a) (toTuple b)

instance Ix Coord where
  range coords = fromTuple <$> range (bimap toTuple toTuple coords)
  inRange coords c = inRange (bimap toTuple toTuple coords) (toTuple c)
  index coords c = index (bimap toTuple toTuple coords) (toTuple c)

type RockPath = [Coord]

type Input = [RockPath]

type OutputA = DisplayString

type OutputB = Void

type Grid s = (STUArray s Coord Char)

------------ UTILS ------------

initialGrid :: forall s. Input -> ST s (Grid s)
initialGrid input = do
  let minY = 0
  let maxY = 2 + maximum (y <$> concat input)
  let minX = 500 - maxY -- (-2) + minimum (x <$> concat input)
  let maxX = 500 + maxY -- 2 + maximum (x <$> concat input)
  newListArray
    (spy (Coord minX minY, Coord maxX maxY))
    (repeat '.')

expandRockPath :: RockPath -> [Coord]
expandRockPath [] = []
expandRockPath [c] = [c]
expandRockPath (from : to : cs) = expandRockLine ++ expandRockPath (to : cs)
  where
    range a b = [a, if a < b then a + 1 else a - 1 .. b]
    expandRockLine
      -- vertical
      | from.x == to.x = (\y -> Coord from.x y) <$> range from.y to.y
      -- horizontal
      | from.y == to.y = (\x -> Coord x from.y) <$> range from.x to.x
      | otherwise = error "not a straight line"

drawRocks :: forall s. Grid s -> [Coord] -> ST s ()
drawRocks grid coords = do
  let writeCoord _ coord = writeArray grid coord '#'
  foldM_ writeCoord () coords

drawRockPath :: forall s. Grid s -> RockPath -> ST s ()
drawRockPath grid path = drawRocks grid (expandRockPath path)

drawRockPaths :: forall s. Grid s -> [RockPath] -> ST s ()
drawRockPaths grid = foldM_ (const $ drawRockPath grid) ()

drawRockBottom :: forall s. Grid s -> ST s ()
drawRockBottom grid = do
  (minC, maxC) <- getBounds grid
  drawRockPath grid [Coord minC.x maxC.y, Coord maxC.x maxC.y]

drawGrid :: Array Coord Char -> [String]
drawGrid grid =
  let (minC, maxC) = bounds grid
   in [[grid ! Coord x y | x <- [minC.x .. maxC.x]] | y <- [minC.y .. maxC.y]]

displayGrid :: [String] -> String
displayGrid = intercalate "\n"

------------ PART A ------------
partA :: Input -> OutputA
partA input = runST $ do
  grid <- initialGrid input
  drawRockPaths grid input
  drawRockBottom grid
  droppedSandUnits <- dropSandUnitsUntilEnd grid 0
  result <- freeze grid
  return $ DisplayString $ show droppedSandUnits ++ "\n" ++ displayGrid (drawGrid result)

dropSandUnitsUntilEnd :: Grid s -> Int -> ST s Int
dropSandUnitsUntilEnd grid unitsDroppedSoFar = do
  status <- dropSandUnit grid
  case status of
    Ended -> return unitsDroppedSoFar
    Running -> dropSandUnitsUntilEnd grid (unitsDroppedSoFar + 1)

data SimulationStatus = Running | Ended deriving (Eq, Show)

dropSandUnit :: forall s. Grid s -> ST s SimulationStatus
dropSandUnit grid = do
  let start = Coord 500 0
  startContent <- readArray grid start
  if startContent /= '.'
    then return Ended
    else do
      sim <- simulateSand grid start
      case sim of
        Just end -> writeArray grid end 'o' $> Running
        Nothing -> return Ended

simulateSand :: forall s. Grid s -> Coord -> ST s (Maybe Coord)
simulateSand grid start = do
  stepResult <- simulateSandStep grid start
  case stepResult of
    Abyss -> return Nothing
    RestAt c -> return $ Just c
    FallTo c -> simulateSand grid c

data SimulationStep = FallTo Coord | RestAt Coord | Abyss

simulateSandStep :: Grid s -> Coord -> ST s SimulationStep
simulateSandStep grid currentCoord = do
  let possibleDestinations = [down, downLeft, downRight] <*> [currentCoord]
  accessibilities <- mapM (determineAccessibility grid) possibleDestinations
  return $ determineStepFromAccessibility currentCoord accessibilities

determineStepFromAccessibility :: Coord -> [Accessibility] -> SimulationStep
determineStepFromAccessibility currentCoord [] = RestAt currentCoord
determineStepFromAccessibility currentCoord (Inaccessible : as) = determineStepFromAccessibility currentCoord as
determineStepFromAccessibility currentCoord (Accessible OutsideGrid : _) = Abyss
determineStepFromAccessibility currentCoord (Accessible (Air coord) : _) = FallTo coord

data Accessibility = Inaccessible | Accessible AccessType

data AccessType = Air Coord | OutsideGrid

determineAccessibility :: forall s. Grid s -> Coord -> ST s Accessibility
determineAccessibility grid newCoord = do
  gridValue <- grid !? newCoord
  return $ case gridValue of
    Nothing -> Accessible OutsideGrid
    Just '.' -> Accessible $ Air newCoord
    _ -> Inaccessible

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
