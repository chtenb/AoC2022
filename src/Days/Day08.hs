{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse foldr/map" #-}
module Days.Day08 (runDay) where

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
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Void
import Data.Array
import Data.Functor ((<&>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

gridSize :: Int
-- gridSize = 5
gridSize = 99

------------ PARSER ------------
inputParser :: Parser Input
inputParser = listArray ((0, 0), (gridSize - 1, gridSize - 1)) . concat <$> parseNestedList

parseNestedList :: Parser IntermediateInput
parseNestedList = do
  many' (toInt <$> digit) `sepBy` "\n"
  where
    toInt d = read [d] :: Int

------------ TYPES ------------
type IntermediateInput = [[Int]]

type Coord = (Int, Int) -- (row, column)

type Input = Array Coord Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = length $ filter (isCoordVisible input) (indices input)

getValueSequence :: (Coord -> Coord) -> Coord -> [Coord]
getValueSequence successor coord =
  if isOutOfBounds coord then [] else coord : getValueSequence successor (successor coord)

toNorth :: Coord -> Coord
toNorth (row, col) = (row, col - 1)

toSouth :: Coord -> Coord
toSouth (row, col) = (row, col + 1)

toEast :: Coord -> Coord
toEast (row, col) = (row + 1, col)

toWest :: Coord -> Coord
toWest (row, col) = (row - 1, col)

isOutOfBounds :: (Int, Int) -> Bool
isOutOfBounds (row, col) = row < 0 || col < 0 || row >= gridSize || col >= gridSize

allSuccessors :: [Coord -> Coord]
allSuccessors = [toNorth, toSouth, toEast, toWest]

isCoordVisible :: Input -> Coord -> Bool
isCoordVisible grid coord = any isVisibleInDirection allSuccessors
  where
    isVisibleInDirection successor = all (\c -> (grid ! c) < currentValue) $ getValueSequence successor (successor coord)
    currentValue = grid ! coord

------------ PART B ------------
partB :: Input -> OutputB
partB input = maximum $ map (scenicScore input) (indices input)

takeWhilePlus1 pred list = part1 ++ take 1 part2
  where
    (part1, part2) = span pred list

scenicScore :: Input -> Coord -> Int
scenicScore grid coord = product $ map scenicScoreForDirection allSuccessors
  where
    scenicScoreForDirection successor = length $ takeWhilePlus1 (\c -> (grid ! c) < currentValue) $ getValueSequence successor (successor coord)
    currentValue = grid ! coord
